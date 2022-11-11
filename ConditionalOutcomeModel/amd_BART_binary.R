# BART conditional outcome model script

setwd("~/scratch/ThresholdsTargetTrial")

library(BART)
library(tidyr)
library(dplyr)

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

# pull out one iteration

set.seed(20220516)
rows <- sample(1:100, size = 1)

X <- X[seq(from = rows, 
                      to = nrow(X), by = 100),]

outcomes <- select(X, admissionid) %>%
            left_join(readRDS("data/amd_eventtimes.rds"),
                      by = "admissionid")

###############################################################

# Death model

X0 <- filter(X, state != 3) %>%
    ungroup() %>%
    select(-admissionid)

outcome0 <- filter(outcomes, state != 3)%>%
    select(admissionid, dateofdeathminute) %>%
    mutate(death28 = (dateofdeathminute < 60*24*28) %in% TRUE) %>%
    select(admissionid, death28)

M0dirichlet <- mc.pbart(x.train = data.matrix(X0),
               y.train = outcome0$death28,
               mc.cores=80,
               printevery=10,
               ntree = 200,
               sparse = TRUE,
               ndpost = 800,
               keepevery = 50)

saveRDS(M0dirichlet, "models/amd_bart_binM0.rds")
#}
gc()

# IMV model

X1 <- filter(X, state %in% c(0,2)) %>%
    ungroup() %>%
    left_join(select(outcomes,admissionid, dateofdeathminute),
              by = "admissionid") %>%
    mutate(death28 = (dateofdeathminute < 60*24*28) %in% TRUE) %>%
    select(-admissionid) %>%
    relocate(death28)

outcome1 <- filter(outcomes, state %in% c(0,2))%>%
    select(admissionid, imv_ever_time) %>%
    mutate(imv28 = (imv_ever_time < 60*24*28) %in% TRUE) %>%
    select(admissionid, imv28)

M1dirichlet <- pbart(x.train = data.matrix(X1),
               y.train = outcome1$imv28,
               printevery=100,
               ntree = 200,
               sparse = TRUE,
               keepevery = 50, 
               ndpost = 800,
               nkeeptreedraws = 800)

saveRDS(M1dirichlet, "models/amd_bart_binM1_death.rds")


gc()


###########################
