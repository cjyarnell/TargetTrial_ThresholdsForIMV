

library(cmdstanr)
library(reshape2)
library(data.table)

for (fold in 1:3){

stem <- "confounder_subset"

D <- 16

files <- c(paste0(stem, fold,  "_chain1.csv")
           , paste0(stem, fold,  "_chain2.csv")
           , paste0(stem, fold,  "_chain3.csv")
           , paste0(stem, fold,  "_chain4.csv")
)

fit <- read_cmdstan_csv(files = files,
                        variables = c("rho",
                                      "alpha",
                                      "sigma",
                                      "offset",
                                      "L_C",
                                      "beta_mu"
                        ),
                        sampler_diagnostics = "")

saveRDS(fit, paste0("fold",fold,"_fit.rds"))

meanrho <- mean(fit$post_warmup_draws[,,grepl("rho", dimnames(fit$post_warmup_draws)$variable)])
meanalpha <- apply(fit$post_warmup_draws[,,grepl("alpha", dimnames(fit$post_warmup_draws)$variable)], 
                   3, mean)
meansigma <- apply(fit$post_warmup_draws[,,grepl("sigma", dimnames(fit$post_warmup_draws)$variable)], 
                   3, mean)
meanoffset <- apply(fit$post_warmup_draws[,,grepl("offset", dimnames(fit$post_warmup_draws)$variable)], 
                    3, mean)
meanL_C <- matrix(apply(fit$post_warmup_draws[,,grepl("L_C", dimnames(fit$post_warmup_draws)$variable)], 
                        3, mean), nrow = D, ncol = D)
meanbeta_mu <- matrix(apply(fit$post_warmup_draws[,,grepl("beta_mu", dimnames(fit$post_warmup_draws)$variable)], 
                            3, mean), ncol = D)

input_mean <- list(rho = meanrho,
                   alpha = meanalpha,
                   sigma = meansigma,
                   offset = meanoffset,
                   L_C = meanL_C,
                   beta_mu = meanbeta_mu)

saveRDS(list(input_mean), paste0("gp_params", 
                                 fold, ".rds"))

}