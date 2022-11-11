# this script interpolates time series and prepares the data for the bart model

setwd("~/scratch/ThresholdsTargetTrial")

dir <- "mimic_simulatedTrajectories"

start <- as.numeric(commandArgs(trailingOnly = TRUE))
cycle <- 400

use_iter <- 200 #number of iterations to use, must be less than n_chains*n_iter
simtraj <- 50 # number of trajectories to simulate per iteration
meanEta <- 1 # number of eta over which to average


M = 20
L = 1.25

#############################

# HSGP functions

lambda <- function(L,m){
  lam = (m*pi/(2*L))^2
  return(lam)
}

spd <- function(alpha, rho, w){
  S = alpha^2 * sqrt(2*pi) * rho * exp(-0.5*(rho^2)*(w^2))
  return(S)
}

phi <- function(L, m, x){
  fi = 1/sqrt(L) * sin(m*pi/(2*L) * (x+L))
  return(fi)
}

############################

library(cmdstanr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tsfeatures)
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

D = 17

pdx_b <- readRDS("data/pdx_b.rds")

files = list(c("confounderOutputs/confounder_subset1_chain1.csv",
              "confounderOutputs/confounder_subset1_chain2.csv",
              "confounderOutputs/confounder_subset1_chain3.csv",
              "confounderOutputs/confounder_subset1_chain4.csv"),
             c("confounderOutputs/confounder_subset2_chain1.csv",
              "confounderOutputs/confounder_subset2_chain2.csv",
              "confounderOutputs/confounder_subset2_chain3.csv",
              "confounderOutputs/confounder_subset2_chain4.csv"),
             c("confounderOutputs/confounder_subset3_chain1.csv",
              "confounderOutputs/confounder_subset3_chain2.csv",
              "confounderOutputs/confounder_subset3_chain3.csv",
              "confounderOutputs/confounder_subset3_chain4.csv"))
#fit <- list(read_cmdstan_csv(files = files[[1]],variables = c("rho","alpha","sigma","offset","L_C","beta_mu"),sampler_diagnostics = "", format = "draws_matrix"),
#            read_cmdstan_csv(files = files[[2]],variables = c("rho","alpha","sigma","offset","L_C","beta_mu"),sampler_diagnostics = "", format = "draws_matrix"),
#            read_cmdstan_csv(files = files[[3]],variables = c("rho","alpha","sigma","offset","L_C","beta_mu"),sampler_diagnostics = "", format = "draws_matrix"))

#saveRDS(fit, "data/confounder_fit_GPparams.rds")

fit <- readRDS("data/confounder_fit_GPparams.rds")

n_chains <- length(files[[1]]) #fit$metadata$num_chains
n_iter <- fit[[1]]$metadata$iter_sampling

params = list()

for(i in 1:3){

rho <- list()
alpha <- list()
sigma <- list()
offset <- list()
L_C <- list()
beta_mu <- list()

shift = 400
    
for (pos in 1:use_iter){
  
  rho[[pos]]    <- as.numeric(fit[[i]]$post_warmup_draws[pos+shift,grepl("rho",dimnames(fit[[i]]$post_warmup_draws)$variable)])
  alpha[[pos]]  <- as.numeric(fit[[i]]$post_warmup_draws[pos+shift,grepl("alpha*",dimnames(fit[[i]]$post_warmup_draws)$variable)])
  sigma[[pos]]  <- as.numeric(fit[[i]]$post_warmup_draws[pos+shift,grepl("sigma*",dimnames(fit[[i]]$post_warmup_draws)$variable)])
  offset[[pos]] <- as.numeric(fit[[i]]$post_warmup_draws[pos+shift,grepl("offset*",dimnames(fit[[i]]$post_warmup_draws)$variable)])
  L_C[[pos]]    <- matrix(as.numeric(
    fit[[i]]$post_warmup_draws[
      pos+shift,grepl("L_C*",dimnames(fit[[i]]$post_warmup_draws)$variable)]), 
                          nrow = D, ncol = D)
  beta_mu[[pos]]<- matrix(as.numeric(
    fit[[i]]$post_warmup_draws[
      pos+shift,grepl("beta_mu*",dimnames(fit[[i]]$post_warmup_draws)$variable)]),
    ncol = D)
  pos <- pos+1
}

params[[i]] = list(rho=rho, alpha=alpha, sigma=sigma, offset=offset, L_C=L_C, beta_mu=beta_mu)
    }

# model for fit / simulation
simulate_mod <- cmdstan_model("models/hsgp_fixedGP.stan")

# load all the patient data for simulation
simulate_dat <- readRDS("data/simulate.rds")

# load more relevant stuff
mean_sd <- readRDS("data/mean_sd.rds") %>%
  filter(variable %in% c("resp_rate",
                         "spo2",
                         "GCS",
                         "fio2",
                         "heart_rate",
                         "sbp",
                         "lactate",
                         "ph",
                         "pco2"))

order <- data.frame(variable = c("sbp", "resp_rate",  "heart_rate", "spo2",  
                                 "fio2",  "lactate" , "ph" ,"pco2", "GCS",
                                 "wob", "pressor", "niv", "hfnc","stdo2",
                                 "icu_dc", "death", "imv"),
                    var = 1:D)

vars <- left_join(order, mean_sd, by = "variable")


# function for parallelization

simulate_trajectory <- function(pt_stay_id){

# for building
# pt_stay_id <- 37507305
id <- filter(simulate_dat$patients, stay_id == pt_stay_id)$id

which_part <- (filter(pdx_b, stay_id == pt_stay_id)$partition %% 3) + 1
    
k_obs <- simulate_dat$k_obs
N_obs <- sum(k_obs == id)
D <- simulate_dat$D
D_c <- simulate_dat$D_c
D_b <- simulate_dat$D_b
x_obs <- simulate_dat$x_obs[k_obs == id]
d_obs <- simulate_dat$d_obs[k_obs == id]
d_obs_matrix <- simulate_dat$d_obs_matrix[k_obs == id,]

B <- simulate_dat$B
dfb <- as.numeric(filter(simulate_dat$dfb, stay_id == pt_stay_id) %>% 
  select(-stay_id))

N_c <- simulate_dat$N_kc[id]
N_b <- simulate_dat$N_kb[id]

start_c <- ifelse(id == 1, 1, sum(simulate_dat$N_kc[1:(id-1)]))
start_b <- ifelse(id == 1, 1, sum(simulate_dat$N_kb[1:(id-1)]))

y_c <- simulate_dat$y_c[(start_c+1):(start_c + N_c)]
y_b <- simulate_dat$y_b[(start_b+1):(start_b + N_b)]

x_pred <- filter(simulate_dat$x_pred, stay_id == pt_stay_id)$time
N_pred <- length(x_pred)
    
rho = params[[which_part]]$rho
alpha = params[[which_part]]$alpha
sigma = params[[which_part]]$sigma
offset = params[[which_part]]$offset
L_C = params[[which_part]]$L_C
beta_mu = params[[which_part]]$beta_mu
    
pt_dat <- list(
  N_iter = use_iter,
  N_obs = N_obs,
  D = D,
  N_baseline = B,
  
  dfb = dfb,
  N_wb = simulate_dat$N_wb,
  which_b = simulate_dat$which_b,
  
  x_obs = x_obs,
  d_obs = d_obs,
  d_obs_matrix = d_obs_matrix,
  
  N_c = N_c,
  D_c = D_c,
  y_c = as.array(y_c),
  
  N_b = N_b,
  D_b = D_b,
  y_b = as.array(y_b),
  
  N_pred = N_pred,
  x_pred = x_pred,
  
  rho = unlist(rho),
  alpha = alpha,
  sigma = sigma,
  offset = offset,
  L_C = L_C,
  beta_mu = beta_mu,
    
  M = M,
  L = L
)
    
simulate_fgp <- simulate_mod$sample(data = pt_dat,
                            refresh = 50,
                            iter_warmup = 200,
                            iter_sampling = meanEta,
                            chains = 1
                            ,validate_csv = F
                                   )

# extract the predictions and eta
chains <- 1
    
# eta first  

eta <- simulate_fgp$draws(variables = "eta", format = "draws_matrix")

eta <- apply(eta, 2, mean)
    
etanames <- list()

    pos = 1
for (d in 1:D){
    for (m in 1:M){
        etanames[[pos]] <- paste0("eta",m, ".",d)
        pos = pos +1
    }
}

eta_names <- unlist(etanames)
    
eta_sim <- data.frame(iter = 1:use_iter,
                      var = rep(eta_names, each = use_iter),
                      eta = eta)
    
saveRDS(eta_sim, paste0(dir, "/etasim",pt_stay_id, ".rds"))


# we can simulate trajectories from the eta
# they can also be simulated within stan
# but the files are large and extracting the 
# coefficients back to R is incredibly slow

# first set up the baseline coefficients    

which_b = simulate_dat$which_b
B <- matrix(data = NA, ncol = 11, nrow = D)

for (d in 1:D){
  B[d,] = dfb[which_b[d,]]
    }

# prediction times
# have to convert to the same timescale used in stan
x_pred <- (seq(from = 0, to = 96, by = 2)/48 - 0.75)/(1)

# do HSGP
Y <-list()
pos = 1
for (i in 1:use_iter){
  
  # make PHI and diagSPD by iteration
  PHI <- matrix(ncol = M, nrow = length(x_pred))
  diagSPD <- rep(NA, M)
  
  for (m in 1:M){
    PHI[,m] = phi(L, m, x_pred)
    diagSPD[m] = sqrt(spd(1.0, rho[[i]], sqrt(lambda(L, m))))
  }
  
  # calculate mean by iteration
  mu_hat <- c(rep(0, D_c),
              offset[[i]]) +
            diag(B %*% beta_mu[[i]])
 # linear covariance by iteration
  alpha_Lc <- t(diag(alpha[[i]]) %*% L_C[[i]])
  
   # pull out the corresponding set of etas
    eta_temp <- filter(eta_sim, iter == i)
    eta_mat <- matrix(data = eta_temp$eta, ncol = D, nrow = M)

    SPD_eta <- diag(diagSPD) %*% eta_mat
    
    f <- matrix(rep(mu_hat, each = length(x_pred)), ncol = D) + 
         PHI %*% SPD_eta %*% alpha_Lc
    
    # all the trajectories use the same model fit hyperparameters 
    # eg 200 iterations of hyperparameters including the basis functions
    # with 50 trajectories for each iteration of hyperparameters
    for (a in 1:simtraj){
         
    y_pred <- data.frame(f)
    names(y_pred) <- order$variable
    
    for (d in 1:D_c){
      y_pred[,d] = rnorm(f[,d],mean = f[,d], sd = sigma[[i]][d])
    }
    for (d in (D_c+1):D){
      y_pred[,d] = rbinom(f[,d], size = 1, prob = inv_logit(f[,d]))
    }
    y_pred$iter = i
    y_pred$traj = a
    y_pred$hour = seq(from = 0, to = 96, by = 2)
    Y[[pos]] <- y_pred
    pos = pos+1
  }
}

Y <- bind_rows(Y)

df_pred <- Y %>%
  pivot_longer(cols = sbp:imv,
               names_to = "variable",
               values_to = "value")

df_pred <- left_join(df_pred, vars, by = "variable") %>%
  mutate(value = ifelse(var <= D_c, value*sd + mean, value))

log_group <- c("resp_rate",
               "heart_rate",
               "sbp",
               "lactate",
               "ph",
               "pco2")

df_pred <- mutate(df_pred,
                  value = ifelse(variable %in% log_group,
                                 exp(value),
                                 value)) %>% 
  mutate(value = ifelse(variable == "spo2",
                        round((inv_logit(value))*101),
                        value)) %>%
  mutate(value = ifelse(variable == "GCS",
                        round((inv_logit(value))*12.2+2.9),
                        value)) %>%
  mutate(value = ifelse(variable == "fio2",
                        round(((inv_logit(value))*79.3+20.9)/5)*5,
                        value))

# thin the blood gas interpolations

df_pred <- df_pred %>%
  group_by(var,iter,traj) %>%
  mutate(rn = row_number()) %>%
  mutate(value = ifelse(var %in% 6:8 & rn %% 4 != 1,
                        NA,
                        value)) %>%
  ungroup() %>%
  select(-rn, -mean, -sd)

df_pred$variable <- factor(df_pred$variable,
                           levels = c("sbp", "resp_rate",  "heart_rate", "spo2",  
                                      "fio2",  "lactate" , "ph" ,"pco2", "GCS",
                                      "wob", "pressor", "niv", "hfnc","stdo2",
                                      "icu_dc", "death", "imv"))

saveRDS(df_pred, file = paste0(dir, "/y_pred",
                               pt_stay_id, ".rds"))
print(paste0("trajectory saved for ", pt_stay_id))

    
#################################################################

# Plot individual simulated trajectories
    
#ribbon_plot <- 
#  df_pred %>%
#  filter(iter == sample(1:use_iter, size = 1), 
#         traj == sample(1:simtraj, size = 1)) %>%
#  mutate(variable = factor(variable, 
#                           labels = c("SBP","RR","HR",
#                                      "SpO2","FiO2",
#                                      "Lactate","pH","pCO2","GCS",
#                                      "Work of breathing","Pressor", 
#                                      "NIV","HFNC", "Standard O2",
#                                      "ICU DC", "Death", "IMV"))) %>%
#  ggplot(aes(x = hour,
#             y = value)) +
#  geom_point(color = "white", size = 0.8) +
#  geom_point(color = c_dark, size = 0.6) +
#  facet_wrap(.~variable, scales = "free_y") +
#  labs(y = "Value (predicted and observed)",
#       x = "Time (hours)",
#       title = paste0("Gaussian process interpolation for patient ",
#                      pt_stay_id)) +
#  theme_minimal()

   
return(pt_stay_id)
}

# load patients

patients <- select(pdx_b, stay_id)

completed <- list.files(paste0(dir,"/"))
completed <- data.frame(filename = completed) %>% 
  mutate(stay_id = as.numeric(gsub(".rds","", gsub("y_pred","",filename))))

patients <- left_join(patients, completed, by = "stay_id") %>%
  filter(is.na(filename))

# parallelize over patients in the fold

print("start parallelization")
ncores = Sys.getenv("SLURM_CPUS_PER_TASK") 
registerDoParallel(cores=60)

#finished_patients <- foreach(k = start:(start + cycle - 1)) %dopar% simulate_trajectory(patients$stay_id[k])
finished_patients <- foreach(k = 1:nrow(patients)) %dopar% simulate_trajectory(patients$stay_id[k])

