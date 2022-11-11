# this script takes in a matrix of all confounder trajectories 
# for a specific patient and outputs the posterior
# distribution of discharge / death by day since eligibility

#SCRATCH wd
setwd("~/scratch/ThresholdsTargetTrial")
source("models/amd_predict_functions.R")

dir = "amd_simulatedTrajectories"
dirPred <- "amd_predictedOutcomes/"

start <- as.numeric(commandArgs(trailingOnly = TRUE))
cycle <- 320

# inv logit
inv_logit <- function(x){exp(x)/(1+exp(x))}
logit <- function(x){log(x/(1-x))}

library(dplyr)
library(tidyr)
library(doParallel)

# patient to stayid lookup
patients <- readRDS("data/amd_simulate.rds")$patients

# completed patients
completed <- list.files(dirPred)
completed <- data.frame(filename = completed) %>% 
  mutate(admissionid = as.numeric(gsub(".rds","", gsub("outcomes","",filename))))

patients <- left_join(patients, completed, by = "admissionid") %>%
  filter(is.na(filename))

# available patients
available <- list.files(dir)
available <- data.frame(filename = available) %>%
  mutate(admissionid = as.numeric(gsub(".rds","", gsub("y_pred","", filename))))

patients <- filter(patients, admissionid %in% available$admissionid)

library(BART)
ncores = 80 
#set_bart_machine_num_cores(ncores)

# info about the size of the trajectories
A = 50
D = 16
N_pred = 49
iter = 200

M = 20
D_c <- 10
D_b <- 3 # don't include the observed events variables (dc, death, imv)

outcome_days <- 28

xtime <- seq(from = 0, 
            to = 96,
            by = 2)

# load bart models
bart_mods <- list(
    M0 = readRDS("models/amd_bart_binM0.rds"), # death28
    M1 = readRDS("models/amd_bart_binM1_death.rds")) # imv28


# load baseline data for the patients
bart_X <- readRDS("data/amd_bart_Xbin.rds") %>%
    select(-admissionid)
 
# names for time series features
etanames <- list()

    pos = 1
for (d in 1:D){
    for (m in 1:M){
        etanames[[pos]] <- paste0("eta",m, ".",d)
        pos = pos +1
    }
}

eta_names <- unlist(etanames)


# rules 
rule_type <- list("SF", "SF", "SF", "SFRR","SFRR","SF","SF", "Delta","Delta","MultiOrgan_Or", "MultiOrgan_And", "UC")
rule_thresholds <- c(98, 88, 110, 25, 35,  98, 98, 1, 0.5,1,1,1)
rule_durations <-  c(  1, 1,   1,  1,  1,   2,  3, 1,   1,1,1,1)

rules <- data.frame(
  name = NA,
  type = unlist(rule_type),
  threshold = rule_thresholds,
  duration = rule_durations)


for(i in 1:(length(rule_type))){
  rules[i,1] <- paste0(rule_type[[i]],".",
                       rule_thresholds[i], ".",
                       rule_durations[i])
}

n_rules <- nrow(rules)

# function to calculate what happens on each trajectory for each rule

temp_outcomes_calc <- function(k_admissionid, y_pred, iterstart, iterdiv){
     
# container for temporary outcomes for each trajectory
      temp_outcomes <- data.frame(admissionid = k_admissionid,
                                  iter = rep(1:iter, each = A*n_rules),
                                  traj = rep(rep(1:A, each = n_rules),iter),
                                  rule = rep(rules$name, A),
                                  state = NA,
                                  obstime = NA,
                                  imv28 = NA,
                                  death28 = NA)
      ind = 1
      for (i in iterstart:(iterstart + iterdiv - 1)){                                              
        for (a in 1:A){
          
            traj <- extract_traj(y_pred, i, a)
            
            for (r in 1:n_rules){
          # safety rules

#          p_mark <- ifelse(pressor(traj) == T,
#                           time_of_pressor(traj),
#                           N_pred+1)

 #         n_mark <- ifelse(GCSthresh(traj) == 1,
 #                          time_of_GCSthresh(traj),
 #                          N_pred+1)

          # respiratory failure rule r

          imv_mark <- imv_rule(traj,r)

          # discharge and death as competing events
          dc_mark <- ifelse(discharge(traj) == 1,
                           time_of_discharge(traj),
                           N_pred+1)

          death_mark <- ifelse(death(traj) == 1,
                           time_of_death(traj),
                           N_pred+1)

          # which event happened first, if any
          marks <- c(death_mark,
                     imv_mark,
  #                   p_mark,
   #                  n_mark,
                     dc_mark)

          end <- min(marks)
          obstime <- min(end, N_pred)- 1
          state <- ifelse(end > N_pred,
                          0,
                          c(3,
                            1,
    #                        "pressor",
     #                       "neuro",
                            2)[which.min(marks)])
          temp_outcomes$state[ind] <- state
          temp_outcomes$obstime[ind] <- obstime*120
          ind = ind + 1
          ind
                
                }
            }
          
          if(i %% 10 == 0){print(paste("patient", k_admissionid, "| iter", i))}

        }
    
    return(temp_outcomes)

}

# function for one trajectory of outcome from the BART object

conditional_outcome_iter <- function(temp_outcomes_vec, bart){
    bartiter <- as.numeric(temp_outcomes_vec[3])
    bartrow  <- as.numeric(temp_outcomes_vec[1])
    rbinom(n = 1, size = 1, 
           p = as.numeric(bart$prob.test[bartiter,
                              bartrow]))
}

# function that takes in a patient id (from 1:number of validation patients)
# and outputs all the outcome iterations for each rule

sim_traj_rules_tsf_pred <- function(k, div){
    
        # k = 1 # for testing
    k_admissionid <- patients$admissionid[k]
    
    y_pred <- load_y_pred(k_admissionid)
        
    print(paste0("loaded trajectories for admissionid ", k_admissionid))
    
    iterstarts <- seq(from = 1, to = iter, by = iter/div)
    iterdiv <- iter/div
    
    temp_outcomes <- foreach(j = 1:div) %dopar% temp_outcomes_calc(k_admissionid, 
                                                                   y_pred, 
                                                                   iterstarts[j], 
                                                                   iterdiv)
    
    temp_outcomes <- bind_rows(temp_outcomes)
    
    # load baseline and time series features 
        X_k <- filter(bart_X, admissionid == k_admissionid) %>%
            ungroup() %>%
            select(-admissionid) %>% 
            slice(rep(1:n(), each = A*n_rules*iter))
        eta <- load_eta(k_admissionid) %>%
             pivot_wider(names_from = var,
                    values_from = eta) %>%
            ungroup() %>%
            rename(itter = iter) %>%
            filter(itter <= iter) %>%
            select(-itter) %>%
            slice(rep(1:n(), each = n_rules*A))
            
        # fill in outcomes based on state, with BART predictions if needed
       # observed death coded as intubation (per protocol analysis)
        temp_outcomes <- temp_outcomes %>% 
            mutate(state = ifelse(state == 3, 1, state))
        X_k$state <- temp_outcomes$state
        X_k$obstime <- temp_outcomes$obstime
    
        Xtest <- data.matrix(bind_cols(X_k, eta))
    
    # predict survival / death
          
    which_death <- which(temp_outcomes$state != 3)
    
    deathpred <- predict(bart_mods[[1]], 
                  newdata = Xtest[which_death,],
                  mc.cores = div)
    
    temp_outcomes$death28[which_death] <- 
        apply(cbind(1:length(which_death), temp_outcomes[which_death,]),1, conditional_outcome_iter, deathpred)                  
    temp_outcomes$death28[temp_outcomes$state == 3] = 1       

    # use that info in predicting imv
    
    which_imv <- which(temp_outcomes$state %in% c(0,2))
        
    imvpred <- predict(bart_mods[[2]], newdata = cbind(temp_outcomes$death28, Xtest)[which_imv,],
                  mc.cores = div)

    temp_outcomes$imv28[which_imv] <- 
        apply( cbind(1:length(which_imv), temp_outcomes[which_imv,]),1, conditional_outcome_iter, imvpred)
        
    temp_outcomes$imv28[temp_outcomes$state == 1] = 1
    #temp_outcomes$imv28[temp_outcomes$state == 3] = 0    
    
    gc()
    
    outcome_file <- paste0("outcomes",
                             patients$admissionid[k], 
                             ".rds")
    final_list <- temp_outcomes %>%
        mutate(state0 = ifelse(state == 0, 1, 0),
               state1 = ifelse(state == 1, 1, 0),
               state2 = ifelse(state == 2, 1, 0),
               state3 = ifelse(state == 3, 1, 0)) %>%
        select(-state) %>%
        group_by(admissionid, iter, rule) %>%
        summarise(cens = mean(state0),
                  imv_obs = mean(state1),
                  dc_obs = mean(state2),
                  death_obs = mean(state3),
                  obs_time = mean(obstime),
                  imv_28 = mean(imv28),
                  death_28 = mean(death28),
                  imvdeath_28 = mean(imv28 == 1 & death28 == 1),
                  imvsurv_28 = mean(imv28 == 1 & death28 == 0),
                  noimvdeath_28 = mean(imv28 == 0 & death28 == 1),
                  noimvsurv_28 = mean(imv28 == 0 & death28 == 0)
                 )
        
    saveRDS(final_list, file = paste0(dirPred,outcome_file))
    print(paste("patient", k, ", admissionid =", k_admissionid, "complete"))
    
    }



# parallelize over patients in the fold
    
print("start parallelization")
                     
div = 1
registerDoParallel(cores=40)

outcome_list <- foreach(k = start:nrow(patients)) %dopar% sim_traj_rules_tsf_pred(k,div)
                 