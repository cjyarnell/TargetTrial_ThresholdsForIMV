## script to make the predictions 
## for confounder model validation
# this does it for a single subset 
# but you can adapt it to loop over all three subsets the way it is done in the paper

setwd("C:/Git/PAHRC/imvThresholdsTargetTrial")
library(cmdstanr)
library(doParallel)

s <- 1

# get the confounder model output
input <- readRDS(paste0(".../gp_params", s, "1.rds"))

# get the timevarying data
validate_dat <- readRDS(paste0(".../validate", s, ".rds"))

# get the model
validate_mod <- cmdstan_model("validate_confounder.stan")

# function to make and extract predictions
validate_one_timestep_one_iter <- function(input, validate_dat,
                                           warmup, samples,
                                           timestep, iter){
  data <- c(validate_dat, input)
  validate_fit <- validate_mod$sample(data = data,
                                      iter_warmup = warmup,
                                      iter_sampling = samples,
                                      chains= 1,
                                      adapt_delta = 0.8,
                                      refresh = 1)
  
  y_pred = as.numeric(unlist(validate_fit$draws(variables = "y_pred")))
  
  rows <- data$N_pred
  outputdf <- data.frame(timestep = rep(timestep, length = rows*samples),
                         sample = rep(1:samples, rows),
                         id = rep(data$k_pred, each = samples),
                         var = rep(data$d_pred, each = samples),
                         value = rep(data$y_obs,each = samples),
                         pred = y_pred
  ) %>%
    left_join(validate_dat$patients, by = "id")
  gc()
  outputdf
}

timesteps = 47

ncores = Sys.getenv("SLURM_CPUS_PER_TASK") 
registerDoParallel(cores=ncores)

x <- foreach(n = 1:timesteps) %dopar% validate_one_timestep_one_iter(
  input = input,
  validate_dat = validate_dat[[n]],
  warmup = 200,
  samples = 200,
  timestep = n
)


# save x, example saveRDS(x, "predictions.rds")

