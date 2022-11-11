###################################################
#
# Pneumonia Acute Hypoxemic Respiratory Failure Cohort
# for the stricter NIV | HFNC & FiO2 > 40 & sat <= 97
# builds cohort database from 2 parquet files:
# PAHRC_Timevarying
# PAHRC_Baseline
# builds a 96h cohort

obs_length = 96

library(tidyverse)
library(caret)
library(arrow)

logit <- function(x){log(x/(1-x))}
inv_logit <- function(x){exp(x)/(1+exp(x))}

setwd("C:/Git/PAHRC/imvThresholdsTargetTrial")
pdx <- read_parquet(
  file = "PAHRC_Timevarying"
)

pdx_b <- read_parquet(
  file = "PAHRC_Baseline"
) 

# remove two patients who have death charted before eligibility
pdx_b <- filter(pdx_b, !(stay_id %in% c(37211828, 36359523)))
pdx <- filter(pdx, !(stay_id %in% c(37211828, 36359523)))

# no continuous measurements including fio2 prior to or including eligibility
pdx_b <- filter(pdx_b, !(stay_id %in% c(34812932)))
pdx <- filter(pdx, !(stay_id %in% c(34812932)))

# remove one patient who has no baseline fio2
pdx_b$stay_id[is.na(pdx_b$fio2_bl)]
pdx_b <- filter(pdx_b, stay_id != 35889708)
pdx <- filter(pdx, stay_id != 35889708)

# have to tidy up the death_obs_time and icu_dc_time variables
# and the death_obs and icu_dc variables
# (make sure no deaths after discharge are included and vice versa)

pdx_b <- pdx_b %>%
  mutate(death_obs = ifelse(death_obs == 1 & icu_dc == 1
                            & death_obs_time > icu_dc_time,
                            0, death_obs),
          icu_dc = ifelse(death_obs == 1 & icu_dc == 1
                         & death_obs_time <= icu_dc_time,
                         0, icu_dc)) %>%
  mutate(death_obs_time = ifelse(icu_dc == 1, NA, death_obs_time),
         icu_dc_time = ifelse(death_obs == 1, NA, icu_dc_time))


# some discharge times are before death which makes no sense
# so put death = discharge time for all patients who died

pdx_b <- pdx_b %>%
  mutate(time_to_discharge = ifelse(!is.na(time_to_death), 
                                    time_to_death,
                                    time_to_discharge)) 


#################################################

# BASELINE

# create transformed version of the baseline data

# factor the admission times

pdx_b$intime <- cut(as.numeric(format(strptime(pdx_b$intime,"%Y-%m-%d %H:%M:%S"),'%H')),
                    breaks = c(0,6,12,18,24), include.lowest = T, right = F)

# discretize continuous variables into quintile binary variables
# we need a way to account for missing at baseline (many variables)
# and these are just for predicting patient means as functions of covariates
# so the discretization allows for nonlinearity and missingness but also captures 
# the essential notion which is that if a variable = x at baseline, 
# the mean will likely be close to x

cvars_bl <- c("preICU_LOS",
              "height",
              "weight_admit",
              "anchor_age",
              "spo2_bl",
              "resp_rate_bl" ,                                           
              "heart_rate_bl",
              "sbp_bl"      ,                                            
              "dbp_bl"       ,                                           
              "mbp_bl"         ,                                         
              "uo_6h_rate_bl"    ,                                       
              "hb_bl"            ,                                       
              "wbc_bl"           ,                                       
              "plt_bl"            ,                                      
              "cr_bl"             ,                                      
              "lactate_bl"         ,                                     
              "pco2_bl"             ,                                    
              "ph_bl"                ,                                   
              "bicarb_bl"             ,                                  
              "trop_bl"                ,                                 
              "alt_bl"     ,                                             
              "glucose_bl"  ,                                            
              "albumin_bl"   ,                                           
              "temperature_bl"
)

bvars_bl <- c("GCS_motor_bl",
              "GCS_verbal_bl",
              "GCS_eyes_bl",
              "wob_bl",
              "cxr_bl", 
              "aline_bl",
              "cvc_bl", 
              "foley_bl", 
              "lasix_bl", 
              "abx_bl", 
              "puffer_bl",
              "opioid_bl", 
              "benzo_bl", 
              "antipsych_bl")

which_cvars <- which(names(pdx_b) %in% cvars_bl)
which_bvars <- which(names(pdx_b) %in% bvars_bl)

pdx_b_tcs <- pdx_b

#discretize into quintiles + missing
for (i in 1:length(which_cvars)){
  pdx_b_tcs[,which_cvars[i]] <- addNA(cut(as.numeric(unlist(pdx_b_tcs[,which_cvars[i]])),
                                          breaks = quantile(as.numeric(unlist(pdx_b_tcs[,which_cvars[i]])),
                                                            p = c(0,0.2,0.4,0.6,0.8,1), na.rm = T),
                                          include_lowest = T))
}

quantile(pdx_b_tcs$fio2_bl, c(0,0.2,0.4,0.6,0.8,1))
mean(pdx_b_tcs$fio2_bl == 66) #fio2 can't be broken into even quintiles
quantile(pdx_b_tcs$fio2_bl, 
         c(0,0.25,0.5,0.75,1))
fio2_cuts <- quantile(pdx_b_tcs$fio2_bl, 
                      c(0,0.25,0.5,0.75,1))
pdx_b_tcs$fio2_bl <- addNA(cut(pdx_b_tcs$fio2_bl, 
                               breaks = fio2_cuts,
                               include.lowest = T))

pdx_b_tcs[which_bvars] <- lapply(pdx_b_tcs[which_bvars], factor, exclude = NULL)

# one-hot encode relevant categoricals for confounder model

pdx_b_tcs <- pdx_b_tcs %>%
  select(stay_id, careunit, anchor_age, gender, ethnicity:insurance,spo2_bl:antipsych_bl)

dmy <- dummyVars("~ .", data = (select(pdx_b_tcs, -stay_id)), fullRank = T)
trsf <- data.frame(predict(dmy, newdata = pdx_b_tcs))
pdx_b_tcs <- bind_cols(select(pdx_b,stay_id), trsf) %>%
  select(-fio2_bl.NA,-spo2_bl.NA)

#################################################

# TIMEVARYING

# combine GCS into one variable

# convert GCS EVM to single score

gcs <- filter(pdx,
              grepl("GCS", variable)) %>%
  group_by(stay_id, time) %>%
  summarise(variable = "GCS",
            value = sum(value))

# add back to main model

pdx <- pdx %>%
  bind_rows(gcs) %>%
  filter(!(variable %in% c("GCS_eyes", "GCS_motor", "GCS_verbal")))

# transformations for continuous variables

log_group <- c("albumin", 
               "alt",
               "bicarb",
               "bili",
               "cr",
               "dbp",
               "glucose",
               "hb",
               "heart_rate",
               "lactate",
               "mbp",
               "pco2",
               "ph",
               "plt",
               "resp_rate",
               "sbp",
               "temperature",
               "trop",
               "uo_6h_rate",
               "wbc")

transform <- function(row, log_group){
  variable <- row[3]
  value <- as.numeric(row[4])
  if(variable %in% log_group){log(value)}
  # use logit transformation functions for GCS, spo2, and fio2
  # because they have a restricted range 
  else if (variable == "GCS"){logit((value-2.9)/12.2)}
  else if (variable == "spo2"){logit(value/101)} # take the floor when transforming back
  else if (variable == "fio2"){logit((value-20.9)/79.3)} # take the floor when transforming back
  else value
}

pdx_transformed <- pdx  
pdx_transformed$value <- apply(pdx, 1, transform, log_group)

# small number of GCS scores were less than 3 (23 total, 0.01%, likely asynchronous charting)
pdx_transformed <- filter(pdx_transformed,
                          !is.na(value))

cont_vars <- c("albumin", 
               "alt",
               "bicarb",
               "bili",
               "cr",
               "dbp",
               "glucose",
               "hb",
               "heart_rate",
               "lactate",
               "mbp",
               "pco2",
               "ph",
               "plt",
               "resp_rate",
               "sbp",
               "temperature",
               "trop",
               "uo_6h_rate",
               "wbc", 
               "spo2",
               "fio2",
               "GCS")


pdx_tcs <- pdx_transformed


# fix icu_dc, death, and pressor variables

# each will be measured once per two hours and whenever it is charted

x_hour <- seq(from = 0, to = 60*obs_length, by = 120)

# make sure no observations after death or discharge
pdx_tcs <- pdx_tcs %>%
  left_join(select(pdx_b, stay_id, death_obs, death_obs_time, icu_dc, icu_dc_time), by = "stay_id") %>%
  filter(
    # no discharge and no death
    (death_obs == 0 & icu_dc == 0) |
      # death   
      (death_obs == 1 & time <= death_obs_time) |
      # discharge 
      (icu_dc == 1 & time <= icu_dc_time)) %>% 
  filter(
    # remove all observations at same time as death
    !(death_obs == 1 & time == death_obs_time & variable != "death")
  ) %>%
  select(-death_obs_time, -icu_dc_time,
         -death_obs, -icu_dc) 

nrow(filter(pdx_tcs, variable == "death", value == 1)) # 195 matches outcome table
nrow(filter(pdx_tcs, variable == "icu_dc", value == 1)) # 1771 matches outcome table

lastobs <- pdx_tcs %>% 
  group_by(stay_id) %>%
  summarise(lastobs = max(time)) 

lastobs2 <- lastobs %>%
  mutate(lastobs = lastobs/60)

saveRDS(lastobs2, "pdx_lastobs.rds")

# pressor, o2 device, and fio2 are only manipulated by clinicians, so
# assume that status is recorded once per two hours and when changed
# and that there is forward fill, consistent with the charting

pressor1 <- data.frame(stay_id = rep(pdx_b_tcs$stay_id, each = length(x_hour)),
                       time = x_hour,
                       variable = "pressor",
                       value = NA) %>%
  left_join(lastobs, by = "stay_id") %>%
  filter(time <= lastobs) %>%
  select(-lastobs)

pressor2 <- filter(pdx_tcs, variable == "pressor") %>%
  bind_rows(pressor1) %>%
  arrange(stay_id, time) %>%
  group_by(stay_id) %>%
  fill(value) %>%
  mutate(value = ifelse(is.na(value), 0, value))

pressor_bl <- filter(pressor2, time <= 0) %>%
  group_by(stay_id) %>%
  summarise(pressor_bl = last(value)) %>%
  mutate(pressor_bl = ifelse(is.na(pressor_bl),0,pressor_bl))

# fill in the NAs that should be 0s in the baseline pressor column
pdx_b_tcs <- select(pdx_b_tcs, -pressor_bl) %>%
  left_join(pressor_bl, by = "stay_id") %>%
  mutate(pressor_bl = ifelse(is.na(pressor_bl), 0, pressor_bl))

pdx_b <- select(pdx_b, -pressor_bl) %>%
  left_join(pressor_bl, by = "stay_id") %>%
  mutate(pressor_bl = ifelse(is.na(pressor_bl), 0, pressor_bl))


fio2a <- data.frame(stay_id = rep(pdx_b_tcs$stay_id, each = length(x_hour)),
                       time = x_hour,
                       variable = "fio2",
                       value = NA) %>%
  left_join(lastobs, by = "stay_id") %>%
  filter(time <= lastobs) %>%
  select(-lastobs)

fio2b <- filter(pdx_tcs, variable == "fio2") %>%
  bind_rows(fio2a) %>%
  arrange(stay_id, time) %>%
  group_by(stay_id) %>%
  fill(value) %>%
  distinct()

o2deva <- data.frame(stay_id = rep(pdx_b_tcs$stay_id, each = length(x_hour)),
                    time = x_hour,
                    variable = "o2_device",
                    value = NA) %>%
  left_join(lastobs, by = "stay_id") %>%
  filter(time <= lastobs) %>%
  select(-lastobs)

o2devb <- filter(pdx_tcs, variable == "o2_device") %>%
  bind_rows(o2deva) %>%
  arrange(stay_id, time) %>%
  group_by(stay_id) %>%
  fill(value) %>%
  distinct()

# add observations for discharge and death

dc_death <- data.frame(stay_id = rep(rep(pdx_b_tcs$stay_id, each = length(x_hour)),2),
                       time = rep(x_hour, 2),
                       variable = rep(c("icu_dc","death"), each = nrow(pdx_b_tcs)),
                       value = 0) %>%
  left_join(lastobs, by = "stay_id") %>%
  filter(time < lastobs) %>%
  select(-lastobs)

pdx_tcs <- filter(pdx_tcs, !(variable %in% c("pressor",
                                             "o2_device",
                                             "fio2"))) %>%
  bind_rows(pressor2, fio2b, o2devb, dc_death)


# binary o2 device version (categorical too slow...)
# the argument is that it indicates all recently used oxygen devices
# it's not perfect but since the latent variables are correlated 
# it is somewhat addressed by the data

niv <- filter(pdx_tcs, variable == "o2_device") %>%
  mutate(variable = "niv",
         value = ifelse(value == 5, 1, 0))

hfnc <- filter(pdx_tcs, variable == "o2_device") %>%
  mutate(variable = "hfnc",
         value = ifelse(value == 4, 1, 0))

stdo2 <- filter(pdx_tcs, variable == "o2_device") %>%
  mutate(variable = "stdo2",
         value = ifelse(value == 3 | 
                          value == 2, 1, 0))

imv <- filter(pdx_tcs, variable == "o2_device") %>%
  mutate(variable = "imv",
         value = ifelse(value == 6, 1, 0))


# other o2 devices and room air is the reference category (0,0,0)

pdx_tcs <- pdx_tcs %>%
  filter(!(variable == "o2_device")) %>%
  bind_rows(niv) %>%
  bind_rows(hfnc) %>%
  bind_rows(stdo2) %>%
  bind_rows(imv)

# fix icu_dc, death, and imv - only one can happen to each patient and 
# when one happens, the others did not happen

temp <- filter(pdx_tcs, variable %in% c("death","imv","icu_dc")) %>%
  pivot_wider(id_cols = c("stay_id","time"),
              names_from = variable,
              values_from = value,
              values_fill = 0) %>%
  arrange(stay_id, time) %>%
  filter(icu_dc == 1 | death == 1 | imv == 1) %>%
  pivot_longer(cols = icu_dc:imv,
               names_to = "variable",
               values_to = "value")

pdx_tcs <- bind_rows(pdx_tcs, temp) %>%
  distinct()

# record the means and standard deviations of 
# transformed variables for reconstruction later
mean_sd <- filter(pdx_tcs, variable %in% cont_vars) %>%
  group_by(variable) %>%
  summarise(mean = mean(value),
            sd = sd(value))

saveRDS(mean_sd, file = "mean_sd.rds")

for (j in 1:nrow(mean_sd)){
  var <- as.character(mean_sd[j,1])
  mean <- as.double(mean_sd[j,2])
  sd <- as.double(mean_sd[j,3])
  
  pdx_tcs <- mutate(
    pdx_tcs,
    value = ifelse(variable == var,
                   (value-mean)/sd,
                   value))
}


# filter out the variables we are not using in the Stan model
# computational constraints limit us to a subset of variables
# maybe one day with a different approximation or Stan model we can include more
# but at present it is not feasible
# take up to 12h before eligibility

pdx_tcs_stan <- filter(pdx_tcs,
                       variable %in% c("resp_rate",
                                       "spo2",
                                       "fio2",
                                       "heart_rate",
                                       "sbp",
                                       "lactate",
                                       "ph",
                                       "pco2",
                                       "wob",
                                       "pressor",
                                       "GCS",
                                       "niv",
                                       "hfnc",
                                       "stdo2",
                                       "icu_dc",
                                       "death",
                                       "imv"),
                       time > -12*60)


#########################################

# Subset for confounder GP model
library(cmdstanr)
library(dplyr)
library(invgamma)

# divide into three partitions for computational reasons

# total patients
K_total <- nrow(pdx_b)
set.seed(20210205)
n_part <- 3
size_part <- ceiling(K_total/n_part)
partition <- sample(rep(1:n_part, size_part))[1:K_total]
size_part <- table(partition)



pdx_b$partition <- partition

# one patient was intubated after ICU discharge 
# but recorded as intubated during ICU admission
# fix that (patient 34354125)

pdx_b <- pdx_b %>%
  mutate(imv = ifelse(stay_id == 34354125, 0, imv))

saveRDS(pdx_b, "pdx_b.rds")
saveRDS(pdx, "pdx.rds")
saveRDS(pdx_b_tcs, "pdx_b_tcs.rds")
saveRDS(pdx_tcs, "pdx_tcs.rds")
saveRDS(pdx_tcs_stan, "pdx_tcs_stan.rds")

for (i in 1:n_part){

  pdx_b_subset <- filter(pdx_b, partition == i)
  subset <- pdx_b_subset$stay_id

  pdx_b_tcs_subset <- filter(pdx_b_tcs,
                           stay_id %in% subset)

  pdx_tcs_stan_subset <- filter(pdx_tcs_stan,
                         stay_id %in% subset)

# organize data for Stan confounder model

M = 20
L = 1.25

cvars <- c("sbp", "resp_rate",  "heart_rate", "spo2",  
           "fio2",  "lactate" , "ph" ,"pco2", "GCS") # continuous variables 
bvars <- c("wob", "pressor", 
           "niv", "hfnc","stdo2",
           "icu_dc", "death", "imv") # binary variables
D_c = length(cvars) # number of continuous variables
D_b = length(bvars) # number of binary variables
D = D_c + D_b

# build baseline predictors of the means
dfb <- pdx_b_tcs_subset %>%
  mutate(id = 1:size_part[i])

patients <- select(dfb, stay_id, id)

# baseline data for predicting time-varying means of confounders
dfb <- dfb[,
           c(1:22,
             26:72,155,
             78:79,
             100:114)] 

N_wb = 11
which_b <- data.frame(matrix(0, nrow = N_wb, ncol = D))
names(which_b) <- c(cvars, bvars)

dem_indices <- 10:15 # put age and sex as predictors for all means
which_b$sbp <- c(dem_indices,43:47) # demographics+ sbp
which_b$resp_rate <- c(dem_indices,33:37)  # demographics + rr
which_b$heart_rate <- c(dem_indices,38:42) # demographics + hr
which_b$spo2 <- c(dem_indices,23:26,30) # demographics + spo2 + niv
which_b$fio2 <- c(dem_indices,27:31) # demographics + fio2 + niv + hfnc
which_b$lactate <- c(dem_indices,73:77) # demographics + lactate
which_b$ph <- c(dem_indices,83:87) # demographics + ph
which_b$pco2 <- c(dem_indices,78:82) # demographics + pco2
which_b$GCS <- c(58:68) # GCS E/V/N
which_b$wob <- c(dem_indices,79:81,71:72) # demographics + wob + high pco2
which_b$pressor <- c(dem_indices,54:57,70) # demographics + pressor + mbp
which_b$niv <- c(dem_indices,30:31,70:72) # demographics + niv/hfnc + high pco2
which_b$hfnc <- c(dem_indices,30:31,70:72) # demographics + niv/hfnc + high pco2
which_b$stdo2 <- c(dem_indices,30:31,70:72) # demographics + niv/hfnc + high fio2
which_b$icu_dc <- c(dem_indices,27:29,70,30) # demographics + fio2 + pressor + niv
which_b$death <- c(dem_indices,30:31,41,76,70) # demographics + high hr / lactate / on pressor
which_b$imv <- c(dem_indices,30:31,70,71,29) # demographics + niv/hfnc + work of breathing + pressor + high fio2

which_b <- which_b - 1

dft <- pdx_tcs_stan_subset %>%
  filter(time <= obs_length*60) %>%
  mutate(var = factor(variable, levels = c(cvars, bvars), labels = 1:D)) %>%
  mutate(time = time/(48*60)) %>%
  left_join(patients, by = "stay_id") %>%
  arrange(id, var, time) %>%
  mutate(var = as.integer(as.character(var))) 

k_obs <- dft$id

# number of observations per patient
N_k <- dft %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  select(n)
N_k <- as.integer(unlist(N_k))

N_kc <- dft %>%
  filter(var <= D_c) %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  select(n)
N_kc <- as.integer(unlist(N_kc))

N_kb <- dft %>%
  filter(var > D_c) %>%
  group_by(id) %>%
  summarise(n = n()) %>%
  select(n)
N_kb <- as.integer(unlist(N_kb))

d_obs <- dft$var
d_obs_matrix <- matrix(0,nrow = nrow(dft), ncol = D)
for (n in 1:nrow(dft)){d_obs_matrix[n,d_obs[n]] = 1}

y_c <- dft$value[dft$var <= D_c]
y_b <- dft$value[dft$var > D_c]

confounder_dat <- list(N_obs = nrow(dft),
                       K = length(unique(dft$id)),
                       D = D,
                       B = ncol(dfb)-1,
                       dfb = data.matrix(select(dfb, -stay_id)),
                       which_b = t(which_b),
                       N_wb = N_wb,
                       N_k = N_k,
                       N_kc = N_kc,
                       N_kb = N_kb,
                       
                       x_obs = dft$time,
                       d_obs = dft$var,
                       d_obs_matrix = d_obs_matrix,
                       k_obs = dft$id,
                       
                       D_c = D_c,
                       N_c = nrow(filter(dft, var <= D_c)),
                       y_c = y_c,
                       c_ind = which(dft$var <= D_c),
                       
                       D_b = D_b,
                       N_b = nrow(filter(dft, var > D_c)),
                       y_b = y_b,
                       b_ind = which(dft$var > D_c),
                       
                       M = M,
                       L = L,
                       

                       patients = patients,
                       dft = dft
)


  filename = paste0("C:/Users/chris/OneDrive/Documents/LargeDataFiles/confounder_data_subset", i, ".json")
  write_stan_json(data = confounder_dat, file = filename)
  saveRDS(confounder_dat, paste0("confounder_subset",i,".rds"))

}
  
# for simulation 
  
  # build baseline predictors of the means
  dfb <- pdx_b_tcs %>%
    mutate(id = 1:K_total)
  
  patients2 <- select(dfb, stay_id, id)
  
  # baseline data for predicting time-varying means of confounders
  # baseline data for predicting time-varying means of confounders
  dfb <- dfb[,
             c(1:22,
               26:72,155,
               78:79,
               100:114)] 
  
  N_wb = 11
  which_b <- data.frame(matrix(0, nrow = N_wb, ncol = D))
  names(which_b) <- c(cvars, bvars)
  
  dem_indices <- 10:15 # put age and sex as predictors for all means
  which_b$sbp <- c(dem_indices,43:47) # demographics+ sbp
  which_b$resp_rate <- c(dem_indices,33:37)  # demographics + rr
  which_b$heart_rate <- c(dem_indices,38:42) # demographics + hr
  which_b$spo2 <- c(dem_indices,23:26,30) # demographics + spo2 + niv
  which_b$fio2 <- c(dem_indices,27:31) # demographics + fio2 + niv + hfnc
  which_b$lactate <- c(dem_indices,73:77) # demographics + lactate
  which_b$ph <- c(dem_indices,83:87) # demographics + ph
  which_b$pco2 <- c(dem_indices,78:82) # demographics + pco2
  which_b$GCS <- c(58:68) # GCS E/V/N
  which_b$wob <- c(dem_indices,79:81,71:72) # demographics + wob + high pco2
  which_b$pressor <- c(dem_indices,54:57,70) # demographics + pressor + mbp
  which_b$niv <- c(dem_indices,30:31,70:72) # demographics + niv/hfnc + high pco2
  which_b$hfnc <- c(dem_indices,30:31,70:72) # demographics + niv/hfnc + high pco2
  which_b$stdo2 <- c(dem_indices,30:31,70:72) # demographics + niv/hfnc + high fio2
  which_b$icu_dc <- c(dem_indices,27:29,70,30) # demographics + fio2 + pressor + niv
  which_b$death <- c(dem_indices,30:31,41,76,70) # demographics + high hr / lactate / on pressor
  which_b$imv <- c(dem_indices,30:31,70,71,29) # demographics + niv/hfnc + work of breathing + pressor + high fio2
  
  which_b <- which_b - 1
  
  
  dft <- pdx_tcs_stan %>%
    filter(time <= 0) %>%
    mutate(var = factor(variable, levels = c(cvars, bvars), labels = 1:D)) %>%
    mutate(time = time/(48*60)) %>%
    left_join(patients2, by = "stay_id") %>%
    arrange(id, var, time) %>%
    mutate(var = as.integer(as.character(var))) 
  
  k_obs <- dft$id
  
  # number of observations per patient
  N_k <- dft %>%
    group_by(id) %>%
    summarise(n = n()) %>%
    select(n)
  N_k <- as.integer(unlist(N_k))
  
  N_kc <- dft %>%
    filter(var <= D_c) %>%
    group_by(id) %>%
    summarise(n = n()) %>%
    select(n)
  N_kc <- as.integer(unlist(N_kc))
  
  N_kb <- dft %>%
    filter(var > D_c) %>%
    group_by(id) %>%
    summarise(n = n()) %>%
    select(n)
  N_kb <- as.integer(unlist(N_kb))
  
  d_obs <- dft$var
  d_obs_matrix <- matrix(0,nrow = nrow(dft), ncol = D)
  for (n in 1:nrow(dft)){d_obs_matrix[n,d_obs[n]] = 1}
  
  y_c <- dft$value[dft$var <= D_c]
  y_b <- dft$value[dft$var > D_c]
  
  x_pred <- data.frame(stay_id = rep(patients2$stay_id, each = length(x_hour)),
                       id = rep(patients2$id, each = length(x_hour)),
                       time = x_hour/(48*60))
  
  N_pred <- nrow(x_pred)
  
  N_k_pred <- x_pred %>%
    group_by(id) %>%
    summarise(n = n()) %>%
    select(n)
  N_k_pred <- as.integer(unlist(N_k_pred))
  
  
  simulate_dat <- list(N_obs = nrow(dft),
                          K = length(unique(dft$id)),
                          D = D,
                          B = ncol(dfb)-1,
                          dfb = dfb,
                          which_b = t(which_b),
                          N_wb = N_wb,
                          N_k = N_k,
                          N_kc = N_kc,
                          N_kb = N_kb,
                          
                          x_obs = dft$time,
                          d_obs = dft$var,
                          d_obs_matrix = d_obs_matrix,
                          k_obs = dft$id,
                          
                          D_c = D_c,
                          N_c = nrow(filter(dft, var <= D_c)),
                          y_c = y_c,
                          c_ind = which(dft$var <= D_c),
                          
                          D_b = D_b,
                          N_b = nrow(filter(dft, var > D_c)),
                          y_b = y_b,
                          b_ind = which(dft$var > D_c),
                          
                          N_pred = N_pred,
                          N_k_pred = N_k_pred,
                          x_pred = x_pred,
                          
                          M = M,
                          L = L,
                       
                          patients = patients2,
                          dft = dft
  )
  
  saveRDS(simulate_dat, "simulate.rds")
  
# for validation
  
  # data for validation of confounder model on test patients
  
  # time 0 to 2h
  hour_segments <- seq(from = 0, to = 92, by = 2)/48 - 0.75
  # prediction windows go from 30min before the hour to 30min after
  # ie the first prediction window is all observations occurring 30-90 minutes after
  # the end of the time series given to the model, which is time 0; the 
  # second prediction window is all observations occurring 90-150 minutes after eligibility,
  # and the time series used to make those predictions ends at 60 minutes after eligibility
  pred_window <- data.frame(start = hour_segments + 1/96,
                            end = hour_segments + 5/96)
  
  validate_dat <- list()
  
  validate_size = 200

for (i in 1:n_part){
    
  pdx_b_subset <- filter(pdx_b, partition == i)
  subset <- pdx_b_subset$stay_id[1:validate_size]
  
  pdx_b_tcs_subset <- filter(pdx_b_tcs,
                             stay_id %in% subset)
  
  pdx_tcs_stan_subset <- filter(pdx_tcs_stan,
                                stay_id %in% subset)
  
  dft_test <- pdx_tcs_stan_subset %>%
    mutate(var = factor(variable, levels = c(cvars, bvars), labels = 1:D)) %>%
    mutate(time = time/(48*60) - 0.75) %>%
    arrange(stay_id, var, time) %>%
    mutate(var = as.integer(as.character(var)))
  
  pos = 1
  for (j in seq_along(hour_segments)){
    
    tempdf_train <- filter(dft_test, time <= hour_segments[j])
    tempdf_pred <- filter(dft_test, 
                          time >= pred_window$start[j] &
                            time < pred_window$end[j])
    
    # only include patients with measurements to predict in the given window
    testpts <- data.frame(
      stay_id = unique(
        tempdf_pred$stay_id[tempdf_pred$stay_id %in% tempdf_train$stay_id]))
    
    N_testpts <- nrow(testpts)
    
    # renumber id's so that it goes from 1 to "number of pts in this dataset"
    testpts <- testpts %>%
      mutate(id = 1:N_testpts)
    
    tempdf_train <- filter(tempdf_train, stay_id %in% testpts$stay_id) %>%
      left_join(testpts, by = "stay_id")
    tempdf_pred <- filter(tempdf_pred, stay_id %in% testpts$stay_id) %>%
      left_join(testpts, by = "stay_id")
    
    k_obs <- tempdf_train$id
    
    # number of observations per patient
    N_k <- tempdf_train %>%
      group_by(id) %>%
      summarise(n = n()) %>%
      select(n)
    N_k <- as.integer(unlist(N_k))
    
    N_kc <- tempdf_train %>%
      filter(var <= D_c) %>%
      group_by(id) %>%
      summarise(n = n()) %>%
      select(n)
    N_kc <- as.integer(unlist(N_kc))
    
    N_kb <- tempdf_train %>%
      filter(var > D_c) %>%
      group_by(id) %>%
      summarise(n = n()) %>%
      select(n)
    N_kb <- as.integer(unlist(N_kb))
    
    d_obs <- tempdf_train$var
    d_obs_matrix <- matrix(0,nrow = nrow(tempdf_train), ncol = D)
    
    # predictions to make per patient
    # number of observations per patient
    N_k_pred <- tempdf_pred %>%
      group_by(id) %>%
      summarise(n = n()) %>%
      select(n)
    N_k_pred <- as.integer(unlist(N_k_pred))
    
    obs_by_k_pred <- group_by(tempdf_pred, id) %>%
      mutate(obs_by_k = row_number())
    
    for (n in 1:nrow(tempdf_train)){d_obs_matrix[n,d_obs[n]] = 1}
    
    y_c <- tempdf_train$value[tempdf_train$var <= D_c]
    y_b <- tempdf_train$value[tempdf_train$var > D_c]
    
    dfb <- filter(pdx_b_tcs_subset, stay_id %in% testpts$stay_id) %>%
      left_join(testpts, by = "stay_id") %>%
      arrange(stay_id)
    
    dfb <- dfb[,
               c(1:22,
                 26:72,155,
                 78:79,
                 100:114)] 
    
    N_wb = 11
    which_b <- data.frame(matrix(0, nrow = N_wb, ncol = D))
    names(which_b) <- c(cvars, bvars)
    
    dem_indices <- 10:15 # put age and sex as predictors for all means
    which_b$sbp <- c(dem_indices,43:47) # demographics+ sbp
    which_b$resp_rate <- c(dem_indices,33:37)  # demographics + rr
    which_b$heart_rate <- c(dem_indices,38:42) # demographics + hr
    which_b$spo2 <- c(dem_indices,23:26,30) # demographics + spo2 + niv
    which_b$fio2 <- c(dem_indices,27:31) # demographics + fio2 + niv + hfnc
    which_b$lactate <- c(dem_indices,73:77) # demographics + lactate
    which_b$ph <- c(dem_indices,83:87) # demographics + ph
    which_b$pco2 <- c(dem_indices,78:82) # demographics + pco2
    which_b$GCS <- c(58:68) # GCS E/V/N
    which_b$wob <- c(dem_indices,79:81,71:72) # demographics + wob + high pco2
    which_b$pressor <- c(dem_indices,54:57,70) # demographics + pressor + mbp
    which_b$niv <- c(dem_indices,30:31,70:72) # demographics + niv/hfnc + high pco2
    which_b$hfnc <- c(dem_indices,30:31,70:72) # demographics + niv/hfnc + high pco2
    which_b$stdo2 <- c(dem_indices,30:31,70:72) # demographics + niv/hfnc + high fio2
    which_b$icu_dc <- c(dem_indices,27:29,70,30) # demographics + fio2 + pressor + niv
    which_b$death <- c(dem_indices,30:31,41,76,70) # demographics + high hr / lactate / on pressor
    which_b$imv <- c(dem_indices,30:31,70,71,29) # demographics + niv/hfnc + work of breathing + pressor + high fio2
    
    which_b <- which_b - 1
    
    validate_dat[[pos]] <- list(
      N_obs = nrow(tempdf_train),
      K = length(unique(tempdf_train$stay_id)),
      D = D,
      B = ncol(dfb)-1,
      dfb = select(dfb, -stay_id),
      which_b = t(which_b),
      N_wb = N_wb,
      N_k = N_k,
      N_kc = N_kc,
      N_kb = N_kb,
      
      x_obs = tempdf_train$time,
      d_obs = tempdf_train$var,
      d_obs_matrix = d_obs_matrix,
      k_obs = tempdf_train$id,
      
      D_c = D_c,
      N_c = nrow(filter(tempdf_train, var <= D_c)),
      y_c = as.array(y_c),
      c_ind = which(tempdf_train$var <= D_c),
      
      D_b = D_b,
      N_b = nrow(filter(tempdf_train, var > D_c)),
      y_b = as.array(y_b),
      b_ind = which(tempdf_train$var > D_c),
      
      N_pred = nrow(tempdf_pred),
      N_k_pred = N_k_pred,
      x_pred = tempdf_pred$time,
      d_pred = tempdf_pred$var,
      y_obs = tempdf_pred$value,
      k_pred = tempdf_pred$id,
      obk_pred = obs_by_k_pred$obs_by_k,
      
      M = M,
      L = L,

      patients = testpts
      
    )
    pos = pos+1
  }
  
  saveRDS(validate_dat, paste0("validate", i, ".rds"))
}
  
  
  ########################################
  
  # Add 28 day ordinal outcome
  
  # 6: never intubated
  # 5: intubated < 7 days and extubated day 28
  # 4: 7 <= intubated < 14 days and extubated on day 28
  # 3: 14 <= intubated but extubated on day 28
  # 2: still intubated day 28
  # 1: death

  pdx_b <- readRDS("pdx_b.rds")
  
  vents <-  read_parquet(
    file = "PAHRC_Ventilation"
  ) %>%
    filter(stay_id %in% pdx_b$stay_id)
  
  
vents_cea<-  
  vents %>%
    group_by(stay_id) %>%
    mutate(from = o2_device,
           to = lead(o2_device),
           start = charttime,
           stop = lead(charttime)) %>%
    filter(from == 6) %>%
    select(-o2_device, -charttime) %>%
    mutate(duration = (stop - start)/(24*60)) %>%
    summarise(duration = sum(duration, na.rm = T)) %>%
  right_join(select(pdx_b, stay_id, gender, anchor_age, imv_time, time_to_death,
                   icu_discharge_time, time_to_discharge),
            by = "stay_id") %>%
    rename(vent_duration = duration) %>%
    mutate(vent_duration = ifelse(is.na(vent_duration), 0, vent_duration)) %>%
    mutate(oxygen_duration = icu_discharge_time/(24*60) - vent_duration,
                           hospital_duration = time_to_discharge/(24*60) - vent_duration - oxygen_duration) %>%
  relocate(stay_id, vent_duration, oxygen_duration, hospital_duration) %>%
  mutate(hospital_duration = ifelse(hospital_duration < 0, 0, hospital_duration),
         imv28 = (imv_time < 28*24*60) %in% TRUE,
         death28 = (time_to_death < 28*24*60) %in% TRUE) %>%
  select(-imv_time, -time_to_death, -icu_discharge_time, -time_to_discharge) %>%
  mutate(hospital_duration = ifelse(is.na(hospital_duration), 0, hospital_duration))

    
saveRDS(vents_cea, "vents_cea.rds")

  # find who is on vent at day 28 
  
vents <- 
  vents %>%
    group_by(stay_id) %>%
    mutate(from = o2_device,
           to = lead(o2_device),
           start = charttime,
           stop = lead(charttime)) %>%
    select(-o2_device, -charttime) %>%
    filter(from == 6, start < 60*24*28) %>% 
    mutate(duration = (stop - start)/(24*60),
           day28vent = (start < 60*24*28) & (stop > 60*24*28)) %>%
    summarise(duration = sum(duration, na.rm = T),
              vent28day = max(day28vent, na.rm = T)) %>%
    arrange(desc(vent28day), desc(duration)) %>%
    mutate(vent_outcome = case_when(
      vent28day == 1 ~ 2,
      duration >= 14 ~ 3,
      duration >= 7  ~ 4,
      duration > 0   ~ 5
    )) %>%
    arrange(stay_id) %>%
  select(stay_id, vent_outcome)
  
outcome <-  pdx_b %>%
    select(stay_id,
           imv_time,
           time_to_death, 
           icu_discharge_time,
           time_to_discharge,
           discharge_location) %>%
    left_join(vents, by = "stay_id") %>%
    mutate(outcome = case_when(
      time_to_death <= 60*24*28 ~ 1,
      is.na(imv_time) & is.na(time_to_death) ~ 6,
      is.na(imv_time) & time_to_death > 60*24*28 ~ 6,
      imv_time > 60*24*28 & time_to_death > 60*24*28 ~ 6,
      TRUE ~ vent_outcome)) %>%
    distinct()

# 9 NA in these outcomes, only in people who had IMV
outcome <- outcome %>% 
  mutate(outcome = ifelse(
    # those who were discharge from icu within 7 days of intubation, outcome = 2
    is.na(outcome) & (icu_discharge_time - imv_time) < 60*24*7 & 
      (icu_discharge_time > imv_time),5,
    outcome)) %>%
  # in patients with outcome otherwise unknown, discharge to hospice before 28 days code as 6
  mutate(outcome = ifelse(
    is.na(outcome) & discharge_location == "HOSPICE"
    & time_to_discharge < 60*24*28, 1, outcome)
  ) %>%
  # for the remaining three, checking the ventilation derived table shows no episodes IMV
  # so classify as 1, never intubated
  mutate(outcome = ifelse(is.na(outcome), 6, outcome)) %>%
  select(stay_id, outcome)

saveRDS(outcome, "pdx_outcome.rds")
