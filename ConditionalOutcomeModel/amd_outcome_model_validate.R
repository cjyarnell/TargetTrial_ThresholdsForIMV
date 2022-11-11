# R script for validating the conditional outcome model
#7425786

setwd("~/scratch/ThresholdsTargetTrial")
library(dplyr)
library(BART)
library(tidyr)
library(precrec)
library(doParallel)

# iterations to average over

iter = 5

# colours
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")
c_blue = "cornflower blue"

bart_df <- readRDS("data/amd_bart_df.rds")

X <- bart_df %>%
    mutate(state = factor(state))

X <- X[seq(from = 1, to = nrow(X), by = 100/iter),]

outcomes <- select(X, admissionid) %>%
            left_join(readRDS("data/amd_eventtimes.rds"),
                      by = "admissionid")

###############################################################

# Death model

X0 <- filter(X, state != 3) %>%
    ungroup() %>%
    select(-admissionid)

X0$num <- rep(1:(nrow(X0)/iter), each = iter)
test <- group_by(X0, num) %>%
    summarise_at(vars(eta1.1:eta20.16),
                ~mean(.x))
X0 <- select(X0,-c(eta1.1:eta20.16)) %>%
    left_join(test, by = "num") %>%
    select(-num) %>%
    distinct()


outcome0 <- filter(outcomes, state != 3)%>%
    select(admissionid, dateofdeathminute) %>%
    mutate(death28 = (dateofdeathminute < 60*24*28) %in% TRUE) %>%
    select(admissionid, death28)

outcome0 <- outcome0[seq(from = 1, to = nrow(outcome0), by = iter),]

iter = 100
N <- nrow(X0)

N_folds = 5
set.seed(20220516)
folds <- sample(1:N_folds, size = N, replace = T)

cv_output <- list()
cv_bl <- list()

ypred <- rep(NA, length( folds))
ypred_bl <- rep(NA, length( folds))

bart_crossval <- function(f){
  xtrain = data.matrix( X0[ folds != f,])
  xtrain_bl = data.matrix( X0[ folds != f,3:46])
  ytrain =  outcome0$death28[ folds != f]
  xtest  = data.matrix( X0[ folds == f,])
  xtest_bl = data.matrix( X0[ folds == f,3:46])
  tv <- mc.pbart(x.train = xtrain,
                 y.train = ytrain,
                 x.test = xtest,
                 mc.cores=2,
                 printevery=100,
                 ntree = 200,
                 sparse = TRUE,
                 ndpost = 800)
  bl <- mc.pbart(x.train = xtrain_bl,
                 y.train = ytrain,
                 x.test = xtest_bl,
                 mc.cores = 2,
                 ntree = 200,
                 printevery = 100,
                 sparse = TRUE,
                 ndpost = 800)
  tvpost <- tv$prob.test.mean  
  blpost <- bl$prob.test.mean
  saveRDS(list(tv = tvpost, 
               bl = blpost,
               outcome = outcome0$death28[folds == f]), 
          paste0("models/amd_bartval",f,".rds"))
    return(1)
}

registerDoParallel(cores=N_folds)
postprob <- foreach(f = 1:N_folds) %dopar% bart_crossval(f)

# generate auc and calibration curves

ypred = list()
ypred_bl = list()
outcome = list()

for (i in 1:N_folds){
  temp <- readRDS(paste0("models/amd_bartval",
                         i, ".rds"))
  ypred[[i]] <- temp$tv
  ypred_bl[[i]] <- temp$bl
  outcome[[i]] <- temp$outcome
}

ypred = unlist(ypred)
ypred_bl = unlist(ypred_bl)
outcome = unlist(outcome)


# compare full model with baseline features only

joined_scores <- join_scores(ypred_bl, ypred)

mdat <- mmdata(joined_scores, outcome, modnames = c("Baseline","Timevarying"))
em <- evalmod(mdat)
auc(em)
test <- autoplot(em, reduce_points = F)
# save plot

data <- data.frame(obs = outcome, 
                   predicted = ypred,
                   #predicted_lb = posterior_lb,
                   #predicted_ub = posterior_ub,
                   decile = cut(ypred, 
                                breaks = quantile(ypred, 
                                                  prob = seq(from = 0, 
                                                             to = 1, 
                                                             length.out = 11)),
                                labels = 1:10,
                                ordered_result = T,
                                include.lowest = T)
) %>%
  group_by(decile) %>%
  summarise( obs_mort = mean(obs),
             pred_mort = mean(predicted),
             pred_lb = quantile(predicted, prob = 0.025),
             pred_ub = quantile(predicted, prob = 0.975))

library(ggplot2)
library(ggpubr)

g<-ggplot(data = data, aes(y = pred_mort, x = obs_mort, 
                             ymin = pred_lb, ymax = pred_ub)) + 
  geom_abline(intercept=0, slope =  1, color = "gray50") + theme_minimal() +
  geom_pointrange(shape = 16, size = 0.5) +
  xlim(c(0,0.85)) + ylim(c(0,0.85)) +
  labs(x = "Observed mortality (proportion)",
       y = "Predicted mortality (proportion)") +
  theme(aspect.ratio = 1)

ggsave("amd_bart_calibration.svg", g, width = 5, height = 4)

#####################################################################################
#
# Check for HTE in IMV
# ie what is the distribution of predicted mortality with and without IMV
# among all the observed time series

# load model

M0 = readRDS("models/amd_bart_binM0.rds")

# first make dataset where everyone who was censored gets intubated at the end of their observation period

X_imv <- X0
X_imv$state[X_imv$state == 0] = 1

pred_imv <- predict(M0, newdata = data.matrix(X_imv), mc.cores = 80)$prob.test.mean

saveRDS(pred_imv, "models/amd_pred_imv.rds")
# now make dataset where everyone intubated was instead censored

X_cens <- X0 
X_cens$obstime[X_cens$state == 1] = 5760
X_cens$state[X_cens$state == 1] = 0

pred_cens <- predict(M0, newdata = data.matrix(X_cens), mc.cores = 80)$prob.test.mean
saveRDS(pred_cens, "models/amd_pred_cens.rds")

# and now the extreme versions
# everyone intubated
X_imv_all <- X0 %>% mutate(state = 1)
all_imv <- predict(M0, newdata = data.matrix(X_imv_all), mc.cores = 80)$prob.test.mean
saveRDS(all_imv, "models/amd_all_imv.rds")

# everyone censored at 5760
X_cens_all <- X0 %>%
    mutate(obstime = 5760, state = 0)
all_cens <- predict(M0, newdata = data.matrix(X_cens_all), mc.cores = 80)$prob.test.mean
saveRDS(all_cens, "models/amd_all_cens.rds")

# everyone discharged at obstime
X_dc <- X0 %>%
    mutate(state = 2)
all_dc <- predict(M0, newdata = data.matrix(X_dc), mc.cores = 80)$prob.test.mean
saveRDS(all_dc, "models/amd_all_dc.rds")


