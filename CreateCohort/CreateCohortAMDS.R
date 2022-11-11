###################################################
#
# Pneumonia Acute Hypoxemic Respiratory Failure Cohort
# for the investigation of outcomes by threshold for invasive ventilation
# builds cohort database for AMDS validation cohort

library(tidyverse)
library(caret)
library(arrow)

# length of observation period
total_hours <- 96


logit <- function(x){log(x/(1-x))}
inv_logit <- function(x){exp(x)/(1+exp(x))}

setwd("C:/Git/PAHRC/imvThresholdsTargetTrial")
amd <- read_parquet(
  file = "AMDS_TT_Timevarying"
)
amd_b <- read_parquet(
  file = "AMDS_TT_admissions"
)
amd_elig <- read_parquet(
  file = "AMDS_TT_Eligibility"
)

head(amd)

length(unique(amd$admissionid)) #1492

# filter charttimes > 7200 (120*60)

amd <- amd %>%
  filter(chartminute < 7200)

# filter everything after invasive ventilation

imv_ever <- amd %>%
  filter(var == "o2_device", value == 20) %>%
  group_by(admissionid) %>%
  summarise(imv_time = first(chartminute, order_by = chartminute))

length(unique(amd$admissionid))
temp <- unique(amd$admissionid)

amd <- amd %>%
  left_join(imv_ever, by = "admissionid") %>%
  filter(((chartminute <= imv_time) %in% TRUE) | (is.na(imv_time))) %>%
  select(-imv_time)

length(unique(amd$admissionid))

# remove everything more than 24h before eligibility
amd <- filter(amd, chartminute > -1440)

# remove / fix impossible values

# fio2 between 21 and 100 keep
# between 0.21 and 1 multiply by 100
# 1 to 21 drop
amd <- amd %>%
  filter(!(var == "fio2" & value > 1 & value < 21)) %>%
  mutate(value = ifelse(var == "fio2" &
                          value <= 1 & value >= 0.21,
                        value*100,value)) %>%
  # drop mbp < 10 or > 200
  filter(!(var == "mbp" & 
             (value < 10 | value > 200))) %>%
  # drop sbp < 10 or > 300
  filter(!(var == "sbp" & 
             (value < 10 | value > 300))) %>%
  # drop ph < 6.5 or > 8
  filter(!(var == "ph" & 
             (value < 6.5 | value > 8))) %>%
  # fix pco2 of 0
  filter(!(var == "pco2" & value < 1)) %>%
  # remove RR > 70 or < 1
  filter(!(var == "resp_rate" & 
             (value < 1 | value > 70))) %>%
  # remove HR 0
  filter(!(var == "heart_rate" & (value < 1 | value > 300))) %>%
  # remove spo2 > 100 or < 1
  filter(!(var == "spo2" & 
             (value > 100 | value < 1))) #%>%
#  group_by(var) %>%
#  summarise(min = min(value, na.rm = T),
#            max = max(value, na.rm = T))

# take out patients with no fio2 measurements (3)

length(unique(amd$admissionid)) #1492

fio2_measured <- amd %>% filter(var == "fio2") %>%
  distinct(admissionid) # 1491 so 1 additional patient had no fio2 measurement after filtering out observations concurrent with IMV

temp <- unique(amd$admissionid)
temp[!(temp %in% unique(fio2_measured$admissionid))]
# 14142 has no fio2 measurements after filtering

amd <- filter(amd,
              admissionid %in% fio2_measured$admissionid)

# exclude some patients with first fio2 < 40 that snuck in due to validated / unvalidated
fio2geq40 <- 
  amd %>%
  group_by(admissionid) %>%
  filter(chartminute <= 0) %>%
  filter(var == "fio2") %>%
  summarise(fio2 = last(value, order_by = chartminute)) %>%
      filter(fio2 >= 40)

temp <- unique(amd$admissionid)
temp[!(temp %in% unique(fio2geq40$admissionid))]


amd <- filter(amd,
              admissionid %in% fio2_measured$admissionid) %>%
  filter(admissionid %in% fio2geq40$admissionid)


# convert pressor measurements to binary on / off

pressor <- 
  amd %>%
    filter(grepl("Noradrenaline", var) |
             grepl("Adrenaline", var) |
             grepl("Dopamine", var)) %>%
    group_by(admissionid, var, chartminute) %>%
    summarise(pressor = sum(value)) %>%
    mutate(pressor = ifelse(pressor == 0, NA, pressor)) %>%
    mutate(value = ifelse(pressor < 0, 0, pressor),
           var = "pressor") %>%
    group_by(admissionid) %>%
    fill(value) %>%
  select(-pressor) %>%
  distinct()

amd <- filter(amd, var %in% c("fio2",
                              "GCS",
                              "heart_rate",
                              "resp_rate",
                              "sbp",
                              "o2_device",
                              "lactate",
                              "pco2",
                              "po2",
                              "ph",
                              "spo2")) %>%
  bind_rows(pressor)

# remove tracheostomy patients
# 4, 18, 19, 11, 13, 14, 15 are all BVM or tracheostomy related

# o2 device counts
amd %>% filter(var == "o2_device") %>%
  mutate(value =  factor(value)) %>%
  group_by(admissionid, value, .drop = F) %>%
  summarise(measured = n() > 0) %>%
  group_by(value) %>%
  summarise(out = sum(measured))

trach <- amd %>% filter(var == "o2_device") %>%
  mutate(value =  factor(value)) %>%
  group_by(admissionid, value, .drop = F) %>%
  summarise(measured = n() > 0) %>%
  group_by(admissionid) %>%
  filter(value %in% c(4, 18:19, 11, 13:15)) %>%
  summarise(out = sum(measured==T)>0) %>%
  filter(out == T)

nrow(trach) # 27
# filter them out

amd <- amd %>%
  filter(!(admissionid %in% trach$admissionid))

imv_ever <- imv_ever %>%
  filter(!(admissionid %in% trach$admissionid)) %>%
  filter(admissionid %in% fio2_measured$admissionid)

patients <- amd %>%
  group_by(admissionid) %>%
  summarise(admissionid = first(admissionid)) %>%
  left_join(imv_ever, by = "admissionid") %>%
  # filter out trach patients
  filter(!(admissionid %in% trach$admissionid)) %>%
  # join with baseline data
  left_join(amd_b, by = "admissionid") %>%
  select(patientid, admissionid, location, admissionyeargroup,
         admissioncount,
         dischargedat, gender, agegroup, dateofdeath, weightgroup, heightgroup, 
         specialty, imv_time) 
  
#########################################
# build baseline table

amd_admissions <- amd_b

amd_b <- 
  read_parquet(
    file = "AMDS_TT_Baseline"
  ) %>%
  # do the same filtering for baseline info
  
  # fio2 between 21 and 100 keep
  # between 0.21 and 1 multiply by 100
  # 1 to 21 drop
  filter(!(var == "fio2" & value > 1 & value < 21)) %>%
  mutate(value = ifelse(var == "fio2" &
                          value <= 1 & value >= 0.21,
                        value*100,value)) %>%
  # drop mbp < 10 or > 200
  filter(!(var == "mbp" & 
             (value < 10 | value > 200))) %>%
  # drop sbp < 10 or > 300
  filter(!(var == "sbp" & 
             (value < 10 | value > 300))) %>%
  # drop ph < 6.5 or > 8
  filter(!(var == "ph" & 
             (value < 6.5 | value > 8))) %>%
  # fix pco2 of 0
  filter(!(var == "pco2" & value < 1)) %>%
  # remove RR > 70 or < 1
  filter(!(var == "resp_rate" & 
             (value < 1 | value > 70))) %>%
  # remove HR 0
  filter(!(var == "heart_rate" & (value < 1 | value > 300))) %>%
  # remove spo2 > 100 or < 1
  filter(!(var == "spo2" & 
             (value > 100 | value < 1))) %>%
  filter(admissionid %in% patients$admissionid) %>%
  select(-rn, -chartminute) %>%
  mutate(var = ifelse(var == "Noradrenaline (Norepinefrine)","norepi",var)) %>%
  mutate(var = ifelse(var == "Adrenaline (Epinefrine)","epi",var)) %>%
  mutate(var = ifelse(var == "Dopamine (Inotropin)","dopa", var)) %>%
  pivot_wider(names_from = var,
              names_sort = TRUE,
              values_from = value) %>%
  group_by(admissionid) %>%
  mutate(norepi = ifelse(is.na(norepi), 0, norepi)) %>%
  mutate(epi = ifelse(is.na(epi), 0, epi)) %>%
  mutate(dopa = ifelse(is.na(dopa), 0, dopa)) %>%
  mutate(pressor = max(c(norepi, epi, dopa), na.rm = T)) %>%
  select(-norepi, -epi, -dopa) %>%
  mutate(sex = ifelse(gender == "Man",0, ifelse(gender == "Vrouw", 1, NA))) %>%
  select(-gender) %>%
  filter(!((GCS_motor < 4) %in% TRUE)) %>%
  filter(!((ph <= 7.20 & pco2 >=60) %in% TRUE)) %>%
  filter(!is.na(fio2)) %>%
  filter(fio2 >= 40) %>%
  mutate(o2_device = case_when(o2_device %in% 16 ~ "NIV",
                   o2_device %in% 17 ~ "NRB"))


sum(is.na(amd_b$o2_device)) # 10 without NRB or NIV as baseline O2 device

amd_b <- amd_b %>% filter(!is.na(o2_device))

patients <- patients %>% filter(admissionid %in% amd_b$admissionid)

# make sure only one admission per patient (take the first one)
patients %>%
  group_by(patientid) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  ungroup() %>% 
  summarise(n(),
            sum(count)) # 86 patients with 179-86 admits

patients <- patients %>%
  group_by(patientid) %>%
  mutate(firstadmit = min(admissioncount)) %>% 
  filter(admissioncount == firstadmit) %>%
  select(-admissioncount, -firstadmit)

amd_b <- filter(amd_b, admissionid %in% patients$admissionid)

amd <- filter(amd, 
              admissionid %in% patients$admissionid)

# filter out ineligible patients from amd

amd <- filter(amd, 
              admissionid %in% amd_b$admissionid)

# find the end-of-observation states
# 0 - still in ICU
# 1 - vented
# 2 - discharge
# 3 - death

amd_eventtimes <- 
  amd_b %>%
    select(admissionid,
           dischargeminute, dateofdeathminute,
           imv_ever_time) %>%
    mutate(state = ifelse(imv_ever_time < total_hours*60, 1, NA)) %>%
    mutate(state = ifelse((dischargeminute <= total_hours*60) &
                            (is.na(dateofdeathminute) | (dateofdeathminute > dischargeminute)) &
                            is.na(state), 2, state)) %>%
    mutate(state = ifelse(is.na(state) & dateofdeathminute < total_hours*60, 3,state)) %>%
    mutate(state = ifelse(is.na(state), 0, state)) %>%
    mutate(eventtime = ifelse(state == 0, total_hours*60,
                              ifelse(state == 1,
                                     imv_ever_time,
                                     dischargeminute)))

# make q2hourly observations for death, imv, and (icu) discharge
# and pressors and oxygen devices and fio2 (and GCS?)

obs_length = 96
x_hour <- seq(from = 0, to = 60*obs_length, by = 120)

# break o2 devices into categories
# 16 = NIV
# 17 = NRB
# 20 = IMV
# all others coded as all 0s for the above
# fill in observations 

o2_device1 <- data.frame(admissionid = rep(amd_b$admissionid, each = length(x_hour)),
                   chartminute = x_hour,
                   var = "o2_device",
                   value = NA)
o2_device2 <- 
  amd %>%
  filter(var == "o2_device") %>%
  bind_rows(o2_device1) %>%
  arrange(admissionid, chartminute) %>%
  group_by(admissionid) %>%
  fill(value) %>%
  distinct() %>%
  left_join(amd_eventtimes, by = "admissionid") %>%
  filter(chartminute <= eventtime) %>%
  select(admissionid:value)

fio2a <- data.frame(admissionid = rep(amd_b$admissionid, each = length(x_hour)),
                    chartminute = x_hour,
                    var = "fio2",
                    value = NA)
fio2b <- 
  amd %>%
  filter(var == "fio2") %>%
  bind_rows(fio2a) %>%
  arrange(admissionid, chartminute) %>%
  group_by(admissionid) %>%
  fill(value) %>%
  distinct() %>%
  left_join(amd_eventtimes, by = "admissionid") %>%
  filter(chartminute <= eventtime) %>%
  select(admissionid:value)

pressor1 <- data.frame(admissionid = rep(amd_b$admissionid, each = length(x_hour)),
                    chartminute = x_hour,
                    var = "pressor",
                    value = NA)
pressor2 <- 
  amd %>%
  filter(var == "pressor") %>%
  bind_rows(pressor1) %>%
  arrange(admissionid, chartminute) %>%
  group_by(admissionid) %>%
  fill(value) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>%
  distinct() %>%
  left_join(amd_eventtimes, by = "admissionid") %>%
  filter(chartminute <= eventtime) %>%
  select(admissionid:value)

icu_dc1 <- data.frame(admissionid = rep(amd_b$admissionid, each = length(x_hour)),
                    chartminute = x_hour,
                    var = "icu_dc",
                    value = 0)
icu_dc2 <- 
  amd_eventtimes %>%
  select(admissionid, state, eventtime) %>%
  rename(chartminute = eventtime,
         value = state) %>%
  mutate(var = "icu_dc") %>%
  select(admissionid, chartminute, var, value) %>%
  mutate(value = ifelse(value == 2, 1, 0)) %>%
  bind_rows(icu_dc1) %>%
  arrange(admissionid, chartminute) %>%
  group_by(admissionid) %>%
  fill(value) %>%
  distinct() %>%
  left_join(amd_eventtimes, by = "admissionid") %>%
  filter(chartminute <= eventtime) %>%
  select(admissionid:value) %>%
  group_by(admissionid, chartminute) %>%
  summarise(admissionid = first(admissionid),
            chartminute = first(chartminute),
            var = "icu_dc",
            value = max(value)) %>%
  ungroup()

death1 <- data.frame(admissionid = rep(amd_b$admissionid, each = length(x_hour)),
                      chartminute = x_hour,
                      var = "death",
                      value = 0)
death2 <- 
  amd_eventtimes %>%
  select(admissionid, state, eventtime) %>%
  rename(chartminute = eventtime,
         value = state) %>%
  mutate(var = "death") %>%
  select(admissionid, chartminute, var, value) %>%
  mutate(value = ifelse(value == 3, 1, 0)) %>%
  bind_rows(death1) %>%
  arrange(admissionid, chartminute) %>%
  group_by(admissionid) %>%
  fill(value) %>%
  distinct() %>%
  left_join(amd_eventtimes, by = "admissionid") %>%
  filter(chartminute <= eventtime) %>%
  select(admissionid:value) %>%
  group_by(admissionid, chartminute) %>%
  summarise(admissionid = first(admissionid),
            chartminute = first(chartminute),
            var = "death",
            value = max(value)) %>%
  ungroup()

# add icu_dc, death, pressor, o2 device to amd

amd <- 
  amd %>%
    filter(!(var %in% c("pressor", "o2_device"))) %>%
    bind_rows(pressor2) %>%
    bind_rows(o2_device2) %>%
    bind_rows(death2) %>%
    bind_rows(icu_dc2) %>%
    arrange(admissionid, chartminute, var)

# then split o2_device into its component categories

niv <- amd %>%
  filter(var == "o2_device") %>%
  mutate(var = "niv",
         value = ifelse(value == 16, 1, 0)) %>%
  distinct()

stdo2 <- amd %>%
  filter(var == "o2_device") %>%
  mutate(var = "stdo2",
         value = ifelse(value == 17, 1, 0)) %>%
  distinct()

imv <- amd %>%
  filter(var == "o2_device") %>%
  mutate(var = "imv",
         value = ifelse(value == 20, 1, 0)) %>%
  distinct()

amd <- amd %>%
  filter(!(var == "o2_device")) %>%
  bind_rows(niv) %>%
  bind_rows(stdo2) %>%
  bind_rows(imv) %>%
  arrange(admissionid, chartminute, var)


amd %>%
  group_by(var) %>%
  summarise(min = min(value, na.rm = T),
            max = max(value, na.rm = T))


# some duplicate measurements at the same time for the same patient

#amd %>% 
#  group_by(admissionid, var, chartminute) %>%
#  filter(!is.na(value)) %>%
#  count() %>%
#  filter(n > 1)
# 1029 / 454791

cont_vars <- c("heart_rate",
               "sbp",
               "mbp",
               "dbp",
               "temperature",
               "po2",
               "pco2",
               "ph",
               "resp_rate",
               "spo2",
               "fio2",
               "GCS",
               "lactate")

# take the mean (continuous) and max (binary) because no other basis to select between them.
# fortunately a small proportion
amd <- amd %>% 
  group_by(admissionid, var, chartminute) %>%
  filter(!is.na(value)) %>%
  summarise(value = ifelse(var %in% cont_vars,
                           mean(value),
                           max(value))) %>%
  ungroup() %>%
  select(admissionid, chartminute, var, value) %>%
  arrange(admissionid, chartminute)

unique(amd$var)

# transform, center, scale

log_group <- c("heart_rate",
               "sbp",
               "po2",
               "pco2",
               "ph",
               "resp_rate",
               "lactate")

transform <- function(row, log_group){
  var <- row[3]
  value <- as.numeric(row[4])
  if(var %in% log_group){log(value)}
  # use logit transformation functions for GCS, spo2, and fio2
  # because they have a restricted range 
  else if (var == "GCS"){logit((value-2.9)/12.2)}
  else if (var == "spo2"){logit(value/101)} # take the floor when transforming back
  else if (var == "fio2"){logit((value-20.9)/79.3)} # take the floor when transforming back
  else value
}

amd_transformed <- amd
amd_transformed$value <- apply(amd, 1, transform, log_group)

cont_vars <- c("heart_rate",
               "sbp",
               "po2",
               "pco2",
               "ph",
               "resp_rate",
               "spo2",
               "fio2",
               "GCS",
               "lactate")

amd_tcs <- amd_transformed

mean_sd <- filter(amd_tcs, var %in% cont_vars) %>%
  group_by(var) %>%
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T))

saveRDS(mean_sd, file = "amd_tt_mean_sd.rds")

for (j in 1:nrow(mean_sd)){
  var2 <- as.character(mean_sd[j,1])
  mean <- as.double(mean_sd[j,2])
  sd <- as.double(mean_sd[j,3])
  
  amd_tcs <- mutate(
    amd_tcs,
    value = ifelse(var == var2,
                   (value-mean)/sd,
                   value))
}


# obs durations per person
lastobs <- amd %>%
  group_by(admissionid) %>%
  summarise(lastobs = max(chartminute))

# prep for stan model and interpolation

amd_tcs_stan <- 
  filter(amd_tcs,chartminute > -12*60) %>% 
  rename(time = chartminute) %>%
  distinct()


amd_tcs_stan %>%
  group_by(admissionid, var) %>%
  summarise(count = n()) %>%
  left_join(lastobs, by = "admissionid") %>%
  mutate(lastobs = lastobs+1) %>%
  group_by(var) %>%
  summarise(mean(lastobs)/mean(count)) %>%
  write_csv(file = "amd_time_btw_obs.csv")

################################################

# get baseline data ready for Stan 

# discretize continuous variables into quintile binary variables
# we need a way to account for missing at baseline (many variables)
# and these are just for predicting patient means as functions of covariates
# so the discretization allows for nonlinearity and missingness but also captures 
# the essential notion which is that if a variable = x at baseline, 
# the mean will likely be close to x

cvars_bl <- c(#  "fio2"      ,                                            
              "heart_rate"    ,                                       
              "lactate"            ,                                       
              "pco2"            ,                                      
              "ph"             ,                                      
              "po2"             ,                                    
              "resp_rate"                ,                                   
              "sbp",                                  
              "spo2"
)

bvars_bl <- c("sex")

which_cvars <- which(names(amd_b) %in% cvars_bl)
which_bvars <- which(names(amd_b) %in% bvars_bl)

amd_b_tcs <- amd_b

#discretize into quintiles + missing
for (i in 1:length(which_cvars)){
  amd_b_tcs[,which_cvars[i]] <- addNA(cut(as.numeric(unlist(amd_b_tcs[,which_cvars[i]])),
                                          breaks = quantile(as.numeric(unlist(amd_b_tcs[,which_cvars[i]])),
                                                            p = c(0,0.2,0.4,0.6,0.8,1), na.rm = T),
                                          include_lowest = T))
}

quantile(amd_b_tcs$fio2, c(0,0.2,0.4,0.6,0.8,1))
quantile(amd_b_tcs$fio2, 
         c(0,0.25,0.5,0.75,1))
fio2_cuts <- quantile(amd_b_tcs$fio2, 
                      c(0,0.25,0.5,0.75,1))
amd_b_tcs$fio2 <- addNA(cut(amd_b_tcs$fio2, 
                               breaks = fio2_cuts,
                               include.lowest = T))


amd_b_tcs <- 
  amd_b_tcs %>%
  select(admissionid, agegroup, fio2, GCS_eyes, GCS_motor, GCS_verbal, 
         heart_rate, lactate, o2_device, pco2, ph, po2, resp_rate, sbp, spo2,
         pressor, sex) %>%
  mutate(GCS = GCS_eyes + GCS_motor + GCS_verbal) %>%
  select(-GCS_eyes, -GCS_verbal, -GCS_motor) %>%
  mutate(GCS = addNA(cut(GCS, breaks = c(3, 8, 10, 12, 14, 15), include_lowest = T))) %>%
  mutate(sex = addNA(sex),
         agegroup = addNA(agegroup)) %>%
  mutate(o2_device = factor(o2_device))

dmy <- dummyVars("~ .", data = (select(amd_b_tcs, -admissionid)), fullRank = T)
trsf <- data.frame(predict(dmy, newdata = amd_b_tcs))
amd_b_tcs <- trsf %>%
  select(-fio2.NA, -agegroup.NA, -sex.0)

# there is one NA value...

amd <- filter(amd, !is.na(value))
amd_tcs <- filter(amd_tcs, !is.na(value))

#########################################

# save 

saveRDS(amd, "amd.rds")
saveRDS(amd_tcs, "amd_tcs.rds")
saveRDS(amd_b, "amd_b.rds")
saveRDS(amd_b_tcs, "amd_b_tcs.rds")
saveRDS(amd_eventtimes, "amd_eventtimes.rds")

#########################################

# organize data for Stan confounder model

M = 20
L = 1.25

cvars <- c("sbp", "resp_rate",  "heart_rate", "spo2",  
           "fio2",  "lactate" , "ph" ,"pco2", "GCS", "po2") # continuous variables 
bvars <- c("pressor", 
           "niv", "stdo2",
           "icu_dc", "death", "imv") # binary variables
D_c = length(cvars) # number of continuous variables
D_b = length(bvars) # number of binary variables
D = D_c + D_b

subset_size = 1279

# build baseline predictors of the means
dfb <- amd_b_tcs %>% mutate(id = 1:nrow(amd_b_tcs))
patients <- select(dfb, admissionid, id)

dfb <- dfb[1:subset_size,]
patients <- patients[1:subset_size,]

# baseline data for predicting time-varying means of confounders

N_wb = 11
which_b <- data.frame(matrix(0, nrow = N_wb, ncol = D))
names(which_b) <- c(cvars, bvars)

dem_indices <- c(2:6, 52) # put age and sex as predictors for all means
which_b$sbp <- c(dem_indices,41:45) # demographics+ sbp
which_b$resp_rate <- c(dem_indices,36:40)  # demographics + rr
which_b$heart_rate <- c(dem_indices,10:14) # demographics + hr
which_b$spo2 <- c(dem_indices,46:50) # demographics + spo2 + niv
which_b$fio2 <- c(dem_indices,7:9,20,51) # demographics + fio2 + niv + hfnc
which_b$lactate <- c(dem_indices,15:19) # demographics + lactate
which_b$ph <- c(dem_indices,26:30) # demographics + ph
which_b$pco2 <- c(dem_indices,21:25) # demographics + pco2
which_b$GCS <- c(dem_indices, 53:57) # GCS E/V/N
which_b$po2 <- c(dem_indices,31:35) # demographics + wob + high pco2
which_b$pressor <- c(dem_indices,41:44,51) # demographics + pressor + mbp
which_b$niv <- c(dem_indices,20, 7:9, 51) # demographics + niv/hfnc + high pco2
which_b$stdo2 <- c(dem_indices,20,7:9, 51) # demographics + niv/hfnc + high fio2
which_b$icu_dc <- c(dem_indices,20,7:9, 51) # demographics + fio2 + pressor + niv
which_b$death <- c(dem_indices,51, 13, 17, 39, 9) # demographics + high hr / lactate / on pressor
which_b$imv <- c(dem_indices,20, 7:9, 51) # demographics + niv/hfnc + work of breathing + pressor + high fio2

which_b <- which_b - 1

dft <- 
  amd_tcs %>%
  filter(admissionid %in% patients$admissionid) %>%
  filter(chartminute <= obs_length*60) %>%
  mutate(var = factor(var, levels = c(cvars, bvars), labels = 1:D)) %>%
  mutate(time = chartminute/(48*60)) %>%
  select(-chartminute) %>%
  left_join(patients, by = "admissionid") %>%
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
                       dfb = data.matrix(select(dfb, -admissionid)),
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

#filename = paste0("C:/Users/chris/OneDrive/Documents/LargeDataFiles/amd_confounder_data_subset.json")
filename = paste0("C:/Users/chris/OneDrive/Documents/LargeDataFiles/amd_confounder_data.json")
library(cmdstanr)
write_stan_json(data = confounder_dat, file = filename)
saveRDS(confounder_dat, "amd_confounder.rds")



# for simulation 

dft <- dft %>%
  filter(time <= 0)

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

x_pred <- data.frame(admissionid = rep(patients$admissionid, each = length(x_hour)),
                     id = rep(patients$id, each = length(x_hour)),
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
                     
                     patients = patients,
                     dft = dft
)

saveRDS(simulate_dat, "amd_simulate.rds")

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

validate_size = 400

subset <- amd_b$admissionid[1:validate_size]
  
amd_b_tcs_subset <- filter(amd_b_tcs,
                             admissionid %in% subset)
  
amd_tcs_stan_subset <- filter(amd_tcs_stan,
                                admissionid %in% subset)
  
  dft_test <- amd_tcs_stan_subset %>%
    mutate(var = factor(var, levels = c(cvars, bvars), labels = 1:D)) %>%
    mutate(time = time/(48*60) - 0.75) %>%
    arrange(admissionid, var, time) %>%
    mutate(var = as.integer(as.character(var)))
  
  pos = 1
  for (j in seq_along(hour_segments)){
    
    tempdf_train <- filter(dft_test, time <= hour_segments[j])
    tempdf_pred <- filter(dft_test, 
                          time >= pred_window$start[j] &
                            time < pred_window$end[j])
    
    # only include patients with measurements to predict in the given window
    testpts <- data.frame(
      admissionid = unique(
        tempdf_pred$admissionid[tempdf_pred$admissionid %in% tempdf_train$admissionid]))
    
    N_testpts <- nrow(testpts)
    
    # renumber id's so that it goes from 1 to "number of pts in this dataset"
    testpts <- testpts %>%
      mutate(id = 1:N_testpts)
    
    tempdf_train <- filter(tempdf_train, admissionid %in% testpts$admissionid) %>%
      left_join(testpts, by = "admissionid")
    tempdf_pred <- filter(tempdf_pred, admissionid %in% testpts$admissionid) %>%
      left_join(testpts, by = "admissionid")
    
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
    
    dfb <- filter(amd_b_tcs_subset, admissionid %in% testpts$admissionid) %>%
      left_join(testpts, by = "admissionid") %>%
      arrange(admissionid)
    
    N_wb = 11
    which_b <- data.frame(matrix(0, nrow = N_wb, ncol = D))
    names(which_b) <- c(cvars, bvars)
    
    dem_indices <- c(2:6, 52) # put age and sex as predictors for all means
    which_b$sbp <- c(dem_indices,41:45) # demographics+ sbp
    which_b$resp_rate <- c(dem_indices,36:40)  # demographics + rr
    which_b$heart_rate <- c(dem_indices,10:14) # demographics + hr
    which_b$spo2 <- c(dem_indices,46:50) # demographics + spo2 + niv
    which_b$fio2 <- c(dem_indices,7:9,20,51) # demographics + fio2 + niv + hfnc
    which_b$lactate <- c(dem_indices,15:19) # demographics + lactate
    which_b$ph <- c(dem_indices,26:30) # demographics + ph
    which_b$pco2 <- c(dem_indices,21:25) # demographics + pco2
    which_b$GCS <- c(dem_indices, 53:57) # GCS E/V/N
    which_b$po2 <- c(dem_indices,31:35) # demographics + wob + high pco2
    which_b$pressor <- c(dem_indices,41:44,51) # demographics + pressor + mbp
    which_b$niv <- c(dem_indices,20, 7:9, 51) # demographics + niv/hfnc + high pco2
    which_b$stdo2 <- c(dem_indices,20,7:9, 51) # demographics + niv/hfnc + high fio2
    which_b$icu_dc <- c(dem_indices,20,7:9, 51) # demographics + fio2 + pressor + niv
    which_b$death <- c(dem_indices,51, 13, 17, 39, 9) # demographics + high hr / lactate / on pressor
    which_b$imv <- c(dem_indices,20, 7:9, 51) # demographics + niv/hfnc + work of breathing + pressor + high fio2
    
    which_b <- which_b - 1
    
    validate_dat[[pos]] <- list(
      N_obs = nrow(tempdf_train),
      K = length(unique(tempdf_train$admissionid)),
      D = D,
      B = ncol(dfb)-1,
      dfb = select(dfb, -admissionid),
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
  
  saveRDS(validate_dat, "amd_validate.rds")
  
  
  
  ########################################
  
# Add 28 day ordinal outcome
  
# 6: never intubated
# 5: intubated < 7 days and extubated day 28
# 4: 7 <= intubated < 14 days and extubated on day 28
# 3: 14 <= intubated but extubated on day 28
# 2: still intubated day 28
# 1: death

  
  vents <-  read_parquet(
    file = "AMDS_Ventilation"
  )
  
  # find who is on vent at day 28 
  
  vents <- 
    vents %>%
    group_by(admissionid) %>%
    mutate(from = o2_device,
           to = lead(o2_device),
           start = chartminute,
           stop = lead(chartminute)) %>%
    select(-o2_device, -chartminute) %>%
    filter(from == 20, start < 60*24*28) %>% 
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
    arrange(admissionid) %>%
    select(admissionid, vent_outcome)
  
  outcome <-  
    amd_eventtimes %>%
    select(admissionid,
           imv_ever_time,
           dischargeminute,
           dateofdeathminute) %>%
    left_join(vents, by = "admissionid") %>%
    mutate(outcome = case_when(
      dateofdeathminute <= 60*24*28 ~ 1,
      is.na(imv_ever_time) & is.na(dateofdeathminute) ~ 6,
      is.na(imv_ever_time) & dateofdeathminute > 60*24*28 ~ 6,
      imv_ever_time > 60*24*28 & dateofdeathminute > 60*24*28 ~ 6,
      TRUE ~ vent_outcome)) %>%
    distinct()
  
  # 118 with NA in these outcomes, only people who had IMV
  outcome <- 
    outcome %>% 
    mutate(outcome = ifelse(
      # those who were discharge from icu within 7 days of intubation, outcome = 2
      is.na(outcome) & (dischargeminute - imv_ever_time) < 60*24*7 & 
        (dischargeminute > imv_ever_time),5,
      outcome)) %>%
    # in patients with end of IMV unknown, assume ICU discharge is similar to end of IMV 
    # based on clinical practice in the Netherlands
    mutate(outcome = case_when(
      is.na(outcome) & 
        (dischargeminute - imv_ever_time) < 60*24*14 & 
        (dischargeminute > imv_ever_time) ~ 4,
      is.na(outcome) & 
        (dischargeminute - imv_ever_time) < 60*24*28 & 
        (dischargeminute > imv_ever_time) ~ 3,
      is.na(outcome) & 
        (dischargeminute - imv_ever_time) >= 60*24*28 & 
        (dischargeminute > imv_ever_time) ~ 2,
      TRUE ~ outcome
    )) %>%
    select(admissionid, outcome)
  
  saveRDS(outcome, "amds_outcome.rds")
  