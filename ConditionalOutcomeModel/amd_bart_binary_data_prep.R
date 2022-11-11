# this script extracts the eta variables from the confounder model HSGP and preps them for the BART model

setwd("~/scratch/ThresholdsTargetTrial")

library(cmdstanr)
library(tidyr)
library(dplyr)
library(doParallel)

logit <- function(x){log(x/(1-x))}
inv_logit <- function(x){exp(x)/(1+exp(x))}

# colours
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

c_blue = "cornflower blue"


eventtimes <- 28
min28day <- 28*24*60
min25day <- 25*24*60

D = 16
use_iter = 100
M = 20

obs_time <- 96*60


amd_b <- readRDS("data/amd_b.rds")

amd_eventtimes <- readRDS("data/amd_eventtimes.rds") %>%
  select(admissionid,
         eventtime, 
         state)

amd_b <- left_join(amd_b, amd_eventtimes,
                   by = "admissionid")

# prepare imv_after_censor variable

amd_b <- mutate(amd_b,
                imv_after_censor = 
                  (imv_ever_time > obs_time & imv_ever_time < min28day) %in% TRUE,
                imv_time = ifelse((imv_time < obs_time) %in% TRUE, imv_time, NA))

# classify 28-day outcomes

amd_b <- mutate(amd_b, 
                death28 = (dateofdeathminute < min28day) %in% TRUE,
                imv28 = (imv_ever_time < min28day) %in% TRUE)

# observation duration (max 4 days)
amd_b <- mutate(amd_b, 
                obstime = eventtime/(60*24))

X <- amd_b %>%
  mutate(location = factor(location, exclude = NA),
         origin = factor(origin, exclude = NULL),
         admissionyeargroup = factor(admissionyeargroup, exclude = NULL),
         agegroup = factor(agegroup),
         weightgroup = factor(weightgroup, exclude = NA),
         heightgroup = factor(heightgroup, exclude = NA),
         specialty = factor(specialty, exclude = NA)) %>%
  select(-dateofdeathminute,
         -destination,
         -imv_ever_time,
         -eventtime,
         - dischargeminute,
         -imv,
         -imv_obs,
         -imv_time,
         -imv28,
         -death28,
         -imv_after_censor) %>%
  arrange(admissionid)

NArows <- c(11, 14, 15, 18, 21, 29, 32, 37)
for (row in seq_along(NArows)){X[is.na(X[,NArows[row]]),NArows[row]] = 0}

X <- X[,c(1,46,47,2:45)]

saveRDS(X, "data/amd_bart_Xbin.rds")

# extract time series features 

files <-  c("confounderOutputs/amd_confounder_chain1.csv")

fit <-     read_cmdstan_csv(files,
                            variables = "eta",
                            format= "draws_list")


big_X <- X %>% slice(rep(1:n(), each = use_iter))

bart_list <- list()

# names for timeseries features
etanames <- list()

pos = 1
for (d in 1:D){
  for (m in 1:M){
    etanames[[pos]] <- paste0("eta",m, ".",d)
    pos = pos +1
  }
}
eta_names <- unlist(etanames)

# combine baseline and time series feature data into one row for each patient
# the order in columns for eta[m,d] is M first then D
# ie eta[1,1], eta[2,1], eta[3,1] ... eta[1,2], eta[2,2], eta[3,2] ...

for (n in 1:nrow(X)){
  temp_id <- n
  temp_admissionid <- X$admissionid[n]
  patt <- paste0("eta\\[", temp_id, ",")
  grab <- grepl(patt, names(fit$post_warmup_draws[[1]]))
  eta_temp <- bind_rows(fit$post_warmup_draws[[1]][grab])
  names(eta_temp) <- eta_names
  bart_list[[n]] <- filter(big_X, admissionid == temp_admissionid) %>%
    bind_cols(eta_temp[1:use_iter,])
}

bart_df <- bind_rows(bart_list)

saveRDS(bart_df, "data/amd_bart_df.rds")