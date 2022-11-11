# this script extracts the eta variables from the confounder model HSGP and preps them for the BART model

setwd("~/scratch/ThresholdsTargetTrial")

use_iter <- 100 #number of iterations to use, must be less than n_chains*n_iter

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

conf <- list(readRDS("data/confounder_subset1.rds"),
             readRDS("data/confounder_subset2.rds"),
             readRDS("data/confounder_subset3.rds"))

pdx_b <- readRDS("data/pdx_b.rds")

D = conf[[1]]$D #vars
M = conf[[1]]$M # basis functions
N = nrow(pdx_b) # patients

# load patients and fix up baseline outcomes to match time to event structure
eventtimes <- 28
min28day <- 28*24*60
min25day <- 25*24*60

# each patient ends observation period in which state
# 0 - still in ICU
# 1 - vented
# 2 - discharge
# 3 - death

pdx_b <- mutate(pdx_b,
                state = ifelse(imv == 1, 1,
                               ifelse(icu_dc == 1, 2,
                                      ifelse(death_obs == 1, 3,0))))

# fix imv_after_censor variable

table(pdx_b$imv_after_censor, pdx_b$imv)

pdx_b <- mutate(pdx_b,
                imv_after_censor = ifelse(imv == 1, 0, imv_after_censor)) %>%
  mutate(imv_after_censor = ifelse((imv_time >= 28*24*60) %in% TRUE, 0, imv_after_censor))

table(pdx_b$imv_after_censor, pdx_b$imv)

# classify 28-day outcomes

pdx_b <- mutate(pdx_b, 
                death28 = time_to_discharge < 28*24*60 & hosp_death == 1,
                imv28 = imv | imv_after_censor)

# observation duration (max 96h)
obstimes <- pdx_b %>%
  select(stay_id, 
         imv, 
         icu_dc,
         death_obs,
         imv_time,
         death_obs_time,
         icu_dc_time) %>%
  mutate(obstime = ifelse(imv == 1, imv_time,
                          ifelse(icu_dc == 1, icu_dc_time,
                                 ifelse(death_obs == 1, 
                                        death_obs_time, 96*60)))) %>%
  select(stay_id, obstime)


# define event times (outcomes are the events)
# time to discharge and time to death are measured from intime
# obstime is measured from eligibility
# eligibility is measured from intime
# follow up is 28 days from eligibility
# so 

pdx_b <- 
  pdx_b %>% 
    mutate(
      eventtime = (time_to_discharge-eligibility_time)/(60*24)
    ) %>%
    mutate(eventtime = ifelse(eventtime > 28, 28, eventtime)) %>%
    left_join(obstimes, by = "stay_id") %>%
    mutate(eventtime = eventtime - obstime/(60*24)) %>%
    mutate(eventtime = ceiling(eventtime)) %>%
# some patients discharged home from ICU
    mutate(eventtime = ifelse(state == 2 & eventtime == 0,
                              1, eventtime))


X <- pdx_b %>%
  mutate(admission_type = factor(admission_type, exclude = NA),
         curr_service = factor(curr_service, exclude = NULL),
         careunit = factor(careunit, exclude = NULL),
         anchor_year_group = factor(anchor_year_group),
         gender = factor(gender, exclude = NA),
         ethnicity = factor(ethnicity, exclude = NA),
         language = factor(language, exclude = NA),
         insurance = factor(insurance, exclude = NA),
         marital_status = factor(marital_status, exclude = NA),
         admission_location = factor(admission_location),
         pressor_bl = ifelse(is.na(pressor_bl),0,pressor_bl)) %>%
  arrange(stay_id)

for (row in 76:85){X[is.na(X[,row]),row] = 0}

X <- X[,c(3,88,92,89, 91, 90, 87, 4:86)]

# identify which confounder model patient corresponds to which baseline info

lookup <- select(pdx_b, partition, stay_id) %>%
    left_join(conf[[1]]$patients, by = "stay_id") %>%
    left_join(conf[[2]]$patients, by = "stay_id") %>%
    left_join(conf[[3]]$patients, by = "stay_id") %>%
    group_by(stay_id) %>%
    mutate(id = max(c(id, id.x, id.y), na.rm = T)) %>%
    select(-id.x, -id.y)

# extract time series features 

files <-  list(c("confounderOutputs/confounder_subset1_chain1.csv"),
             c("confounderOutputs/confounder_subset2_chain1.csv"),
             c("confounderOutputs/confounder_subset3_chain1.csv"))

fit <-list(read_cmdstan_csv(files[[1]],
                       variables = "eta",
                      format= "draws_list"),
           read_cmdstan_csv(files[[2]],
                       variables = "eta",
                      format= "draws_list"),
           read_cmdstan_csv(files[[3]],
                       variables = "eta",
                      format= "draws_list"))


X <- left_join(X, lookup, by = c("stay_id","partition")) %>%
arrange(partition, id)
saveRDS(X, "data/bart_Xbin.rds")

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
    temp_id <- X$id[n]
    temp_part <- X$partition[n]
    temp_stay_id <- X$stay_id[n]
    patt <- paste0("eta\\[", temp_id, ",")
    grab <- grepl(patt, names(fit[[temp_part]]$post_warmup_draws[[1]]))
    eta_temp <- bind_rows(fit[[temp_part]]$post_warmup_draws[[1]][grab])
    names(eta_temp) <- eta_names
    bart_list[[n]] <- filter(big_X, stay_id == temp_stay_id) %>%
        bind_cols(eta_temp[1:use_iter,])
}

bart_df <- bind_rows(bart_list)

saveRDS(bart_df, "data/bart_df.rds")
