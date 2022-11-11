
## this file processes the confounder model validation run for AmsterdamUMCdb

setwd("C:/Git/PAHRC/imvThresholdsTargetTrial")

file <- paste0("C:/Users/chris/OneDrive/Documents/LargeDataFiles/amd_predictions.rds")
  
vars <- data.frame(var = 1:16,
                   variable = c("sbp", "resp_rate",  "heart_rate", "spo2",  
           "fio2",  "lactate" , "ph" ,"pco2", "GCS", # continuous variables 
           "po2", "pressor", 
           "niv", "stdo2",
           "icu_dc", "death", "imv"))

mean_sd <- readRDS("amd_tt_mean_sd.rds") %>%
  rename(variable = var) %>%
  filter(variable %in% vars$variable) %>%
  left_join(vars, by = "variable")
  
library(cmdstanr)
library(tidyverse)
library(bayesplot)
library(kableExtra)
library(ggpubr)
library(posterior)
library(reshape2)
library(ggplot2)
library(yardstick)
  
# colours
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")

c_blue = "cornflower blue"

# inv logit
inv_logit <- function(x){exp(x)/(1+exp(x))}
logit <- function(x){log(x/(1-x))}

# names of each variable

iter <- 100

validation_output_fold <- readRDS(file)

bigdf <- bind_rows(validation_output_fold)

rm(validation_output_fold)

############################
# Continuous variables

# convert continuous variables back
tempmean <- function(var){
  varnum <- which(mean_sd$variable == var)
  mean_sd$mean[varnum]
}

tempsd <- function(var){
  varnum <- which(mean_sd$variable == var)
  mean_sd$sd[varnum]
}

# sbp
bigdf  <- mutate(bigdf,
                 value = ifelse(var == 1,
                                exp(value*tempsd("sbp")
                                    + tempmean("sbp")),
                                value),
                 pred = ifelse(var == 1,
                               exp(pred*tempsd("sbp")
                                   + tempmean("sbp")),
                               pred))
bigdf  <- mutate(bigdf,
                 value = ifelse(var == 2,
                                exp(value*tempsd("resp_rate")
                                    + tempmean("resp_rate")),
                                value),
                 pred = ifelse(var == 2,
                               exp(pred*tempsd("resp_rate")
                                   + tempmean("resp_rate")),
                               pred))
bigdf  <- mutate(bigdf,
                 value = ifelse(var == 3,
                                exp(value*tempsd("heart_rate")
                                    + tempmean("heart_rate")),
                                value),
                 pred = ifelse(var == 3,
                                   + tempmean("heart_rate")),
                               pred))
bigdf  <- mutate(bigdf,
                 value = ifelse(var == 4,
                                101*inv_logit(
                                  value*tempsd("spo2") + 
                                    tempmean("spo2")),
                                value),
                 pred = ifelse(var == 4,
                               101*inv_logit(
                                 pred*tempsd("spo2") + 
                               exp(pred*tempsd("heart_rate")
                                   tempmean("spo2")),
                               pred))
bigdf  <- mutate(bigdf,
                 value = ifelse(var == 5,
                                floor(20.9+79.3*inv_logit(
                                  value*tempsd("fio2") + 
                                    tempmean("fio2"))),
                                value),
                 pred = ifelse(var == 5,
                               floor(20.9+79.3*inv_logit(
                                 pred*tempsd("fio2") + 
                                   tempmean("fio2"))),
                               pred))
bigdf  <- mutate(bigdf,
                 value = ifelse(var == 6,
                                exp(value*tempsd("lactate")
                                    + tempmean("lactate")),
                                value),
                 pred = ifelse(var == 6,
                               exp(pred*tempsd("lactate")
                                   + tempmean("lactate")),
                               pred))
bigdf  <- mutate(bigdf,
                 value = ifelse(var == 7,
                                exp(value*tempsd("ph")
                                    + tempmean("ph")),
                                value),
                 pred = ifelse(var == 7,
                               exp(pred*tempsd("ph")
                                   + tempmean("ph")),
                               pred))
bigdf  <- mutate(bigdf,
                 value = ifelse(var == 8,
                                exp(value*tempsd("pco2")
                                    + tempmean("pco2")),
                                value),
                 pred = ifelse(var == 8,
                               exp(pred*tempsd("pco2")
                                   + tempmean("pco2")),
                               pred))
bigdf  <- mutate(bigdf,
                 value = ifelse(var == 9,
                                2.9+12.2*inv_logit(
                                  value*tempsd("GCS") + 
                                    tempmean("GCS")),
                                value),
                 pred = ifelse(var == 9,
                               2.9+12.2*inv_logit(
                                 pred*tempsd("GCS") + 
                                   tempmean("GCS")),
                               pred))

bigdf  <- mutate(bigdf,
                 value = ifelse(var == 10,
                                exp(value*tempsd("po2")
                                    + tempmean("po2")),
                                value),
                 pred = ifelse(var == 10,
                               exp(pred*tempsd("po2")
                                   + tempmean("po2")),
                               pred))

steps <- seq(from = 1, to = 95, by = 2)
cuts <- seq(from = 0, to = 96, by = 2)

bigdf <- bigdf %>%
  mutate(time = (time + 0.75)*48) %>%
  mutate(timestep = cut(time, ordered = T, breaks = cuts)) %>%
  mutate(timestep = as.integer(timestep)*2)

# takes about 30s
mse_cov <- filter(bigdf, var <= 10) %>%
  mutate(err2 = (pred-value)^2) %>%
  group_by(timestep,sample,var) %>%
  summarise(mean_obs = mean(value),
            mean_pred = mean(pred),
            coverage = mean(value > quantile(pred, 0.025) &
                              value < quantile(pred, 0.975)),
            rmse = sqrt(mean(err2)),
            count_sample_var = n()) %>%
  ungroup() %>%
  group_by(timestep,var) %>%
  summarise(rmse_mean = mean(rmse),
            rmse95lb = quantile(rmse, 0.025),
            rmse95ub = quantile(rmse, 0.975),
            cover = mean(coverage),
            cover95lb = quantile(coverage, 0.025),
            cover95ub = quantile(coverage, 0.975),
            count = first(count_sample_var))

maxcount = max(mse_cov$count)
maxrmse = max(mse_cov$rmse95ub)

var_labeller <- function(variable, value){
  return(vars$variable[value])
}

plot_rmse <- 
  ggplot(data = mse_cov,
       aes(x = timestep)) +
  geom_col(aes(y = count*maxrmse/maxcount),
             size = 0.8, col = "grey90",
           fill = "grey90") +
  geom_ribbon(aes(y = rmse_mean,
                  ymin = rmse95lb,
                  ymax = rmse95ub),
              alpha = 0.5, fill = c_dark) +
  geom_line(aes(y = rmse_mean),
            col = "white", size = 1) +
  geom_line(aes(y = rmse_mean),
            col = "black", size = 0.8) +
  scale_y_continuous(name = "RMSE",
                     sec.axis = sec_axis(~.*maxcount/maxrmse,
                                         name = "Observation count")) +
  theme_minimal() +
  facet_wrap(.~var, labeller = var_labeller,
             scales = "free_y")

plot_cover <- ggplot(data = mse_cov,
       aes(x = timestep,
           y = cover,
           ymin = cover95lb,
           ymax = cover95ub)) +
  geom_col(aes(y = count/maxcount),
           size = 0.8, col = "grey90",
           fill = "grey90") +
  geom_ribbon(alpha = 0.5, fill = c_dark) +
  geom_line(col = "black", size = 1) +
  scale_y_continuous(name = "Coverage",
                     sec.axis = sec_axis(~.*maxcount,
                                         name = "Observation count")) +
  theme_minimal() +
  facet_wrap(.~var, labeller = var_labeller)

ggsave("plots/amd_confounder_rmse.svg", plot_rmse, height = 6, width = 8)
ggsave("plots/amd_confounder_coverage.svg", plot_cover, height = 6, width = 8)

contmean <- filter(bigdf, var <= 10) %>%
  group_by(timestep,sample,var) %>%
  summarise(mean_obs = mean(value),
            mean_pred_sample = mean(pred),
            count_sample_var = n()) %>%
  ungroup() %>%
  group_by(timestep,var) %>%
  summarise(mean_obs = first(mean_obs),
            mean_pred = mean(mean_pred_sample),
            pred95lb = quantile(mean_pred_sample, 0.025),
            pred95ub = quantile(mean_pred_sample, 0.975),
            count = first(count_sample_var))

plot_cmean <- ggplot(data = contmean,
                     aes(x = timestep,
                         y = mean_pred,
                         ymin = pred95lb,
                         ymax = pred95ub)) +
  geom_ribbon(alpha = 0.5, fill = c_blue) +
  geom_line(col = "black", size = 1) +
  geom_point(aes(y = mean_obs),
             col = "white", size = 1) +
  geom_point(aes(y = mean_obs),
             col = "red", size = 0.8) +
  theme_minimal() +
  facet_wrap(.~var, labeller = var_labeller,
             scales = "free") +
  labs(y = "Mean (observed and predicted)")

ggsave("plots/amd_confounder_cmean.svg", plot_cmean,
       height = 6, width = 8)

# Overall RMSEs 

mse_cov_tbl <- filter(bigdf, var <= 10) %>%
  mutate(err2 = (pred-value)^2) %>%
  group_by(timestep,sample,var) %>%
  summarise(mean_obs = mean(value),
            mean_pred = mean(pred),
            coverage = mean(value > quantile(pred, 0.025) &
                              value < quantile(pred, 0.975)),
            rmse = sqrt(mean(err2)),
            count_sample_var = n()) %>%
  ungroup() %>%
  group_by(var) %>%
  summarise(rmse_mean = mean(rmse),
            rmse95lb = quantile(rmse, 0.025),
            rmse95ub = quantile(rmse, 0.975),
            cover = mean(coverage),
            cover95lb = quantile(coverage, 0.025),
            cover95ub = quantile(coverage, 0.975),
            count = sum(count_sample_var))

contmean_tbl <- filter(bigdf, var <= 10) %>%
  group_by(timestep,sample,var) %>%
  summarise(mean_obs = mean(value),
            mean_pred_sample = mean(pred),
            count_sample_var = n()) %>%
  ungroup() %>%
  group_by(var) %>%
  summarise(mean_obs = first(mean_obs),
            mean_pred = mean(mean_pred_sample),
            pred95lb = quantile(mean_pred_sample, 0.025),
            pred95ub = quantile(mean_pred_sample, 0.975),
            count = sum(count_sample_var))

mse_cov_tbl
contmean_tbl

#############################
# Binary variables

bmean <- bigdf %>%
  filter(var > 10) %>%
  group_by(timestep, var, sample) %>%
  summarise(mean_pred = mean(pred),
            mean_value = mean(value), 
            count_sample_var = n()) %>%
  ungroup() %>%
  group_by(timestep, var) %>%
  summarise(mean_postpred = mean(mean_pred),
            postpred95lb = quantile(mean_pred, 0.025),
            postpred95ub = quantile(mean_pred, 0.975),
            mean_value = first(mean_value),
            events = first(count_sample_var)*first(mean_value))

bmean_tbl <- bigdf %>%
  filter(var > 10) %>%
  group_by(var) %>%
  summarise(mean_value = mean(value),
            mean_postpred = mean(pred)) 
bmean_tbl

maxevents = max(bmean$events)
maxprob = max(max(bmean$postpred95ub))

plot_binpred <- ggplot(data = bmean,
       aes(x = timestep)) +
  geom_col(aes(y = events*maxprob/maxevents),
           size = 0.8, col = "grey90",
           fill = "grey90") +
  geom_ribbon(aes(y = mean_postpred,
                  ymin = postpred95lb,
                  ymax = postpred95ub),
              alpha = 0.5, fill = c_blue) +
  geom_line(aes(y = mean_postpred),
            col = "white", size = 1) +
  geom_line(aes(y = mean_postpred),
            col = "black", size = 0.8) +
  geom_point(aes(y = mean_value),
             col = "white", size = 1) +
  geom_point(aes(y = mean_value),
             col = "red", size = 0.8) +
  scale_y_continuous(name = "Mean (predicted and observed)",
                     sec.axis = sec_axis(~.*maxevents/maxprob,
                                         name = "Event count")) +
  theme_minimal() +
  facet_wrap(.~var, labeller = var_labeller)

ggsave("plots/amd_confounder_binpred.svg", plot_binpred,
       width = 8, height = 6)

disc <- bigdf %>%
  filter(var > 10) %>%
  group_by(timestep, var, id) %>%
  summarise(pred = mean(pred),
            value = first(value)) %>%
  mutate(value = factor(value)) %>%
  group_by(timestep, var) %>%
  roc_auc(truth = value,
          estimate = pred,
          event_level = "second")

prec <- bigdf %>%
  filter(var > 10) %>%
  group_by(timestep, var, id) %>%
  summarise(pred = mean(pred),
            value = first(value)) %>%
  mutate(value = factor(value)) %>%
  group_by(timestep, var) %>%
  pr_auc(truth = value,
          estimate = pred,
          event_level = "second")

rocprc <- bind_rows(disc, prec) %>%
  left_join(select(cal, timestep, var, events),
            by = c("timestep","var")) %>%
  mutate(events = events/2,
         .metric = factor(.metric, labels = c("PRC","ROC")))
  
conf_auc <- ggplot(data = rocprc,
       aes(x = timestep,
           y = .estimate,
           color = .metric)) +
  geom_col(aes(y = events/maxevents),
           size = 0.8, col = "grey90",
           fill = "grey90") +
  geom_point(color = "white", size = 1) +
  geom_point(alpha = 1, size = 0.8) +
  scale_y_continuous(name = "Mean (predicted and observed)",
                     sec.axis = sec_axis(~.*maxevents,
                                         name = "Event count")) +
  scale_color_manual(values = c(c_blue, c_dark), 
                     name = "AUC") +
  theme_minimal() +
  facet_wrap(.~var, labeller = var_labeller)

disc_tbl <- bigdf %>%
  filter(var > 10) %>%
  group_by(timestep, var, id) %>%
  summarise(pred = mean(pred),
            value = first(value)) %>%
  mutate(value = factor(value)) %>%
  group_by(var) %>%
  roc_auc(truth = value,
          estimate = pred,
          event_level = "second")

prec_tbl <- bigdf %>%
  filter(var > 10) %>%
  group_by(timestep, var, id) %>%
  summarise(pred = mean(pred),
            value = first(value)) %>%
  mutate(value = factor(value)) %>%
  group_by(var) %>%
  pr_auc(truth = value,
         estimate = pred,
         event_level = "second")

rocprc_tbl <- left_join(disc_tbl, prec_tbl,
                    by = "var") %>%
  select(var, .estimate.x, .estimate.y) %>%
  mutate(roc = .estimate.x,
         prc = .estimate.y) %>%
  select(var, roc, prc)

left_join(rocprc_tbl,bmean_tbl,by = "var") %>%
  mutate(roc = round(roc,3),
         prc = round(prc, 3),
         postmean = round(mean_postpred,4),
         obsmean = round(mean_value, 4)) %>%
  mutate(Variable = vars$variable[var]) %>%
  select(Variable, obsmean, postmean, roc, prc) %>%
  write.csv(file = "amd_ROCPRC_bmean_Tbl.csv")

left_join(mse_cov_tbl, contmean_tbl, by = "var") %>%
  mutate(RMSE = paste0(
    round(rmse_mean,2),
                       " (",
    round(rmse95lb, 2),
                       " to ",
    round(rmse95ub,2),
                       ")"),
         Coverage = paste0(
           round(cover,2), " (",
           round(cover95lb,2), " to ",
           round(cover95ub,2), ")"
         ),
         obsmean = round(mean_obs,2),
         postmean = paste0(round(mean_pred,2),
                           " (",
                           round(pred95lb, 2),
                           " to ",
                           round(pred95ub, 2),
                           ")")) %>%
  mutate(Variable = vars$variable[var]) %>%
  select(Variable, obsmean, postmean, RMSE, Coverage) %>%
  write.csv(file = "amd_RMSE_cmean_Tbl.csv")

##################################
#
# Single patients

# individual trajectory facet plot

plot_cvar <- function(vari,df, endtime){
  ggplot(data = filter(df, var == vari),
         aes(x = time)) +
    geom_ribbon(aes(y = mean_pred,
                    ymin = pred95lb,
                    ymax = pred95ub),
                alpha = 0.3, fill = c_blue) +
    geom_ribbon(aes(y = mean_pred,
                    ymin = pred50lb,
                    ymax = pred50ub),
                alpha = 0.5, fill = c_blue) +
    geom_line(aes(y = mean_pred),
              size = 1.2, color = "white") +
    geom_line(aes(y = mean_pred),
              size = 0.8, color = "black") +
    geom_point(aes(y = value),
               size = 1.2, color = "white") + 
    geom_point(aes(y = value),
               size = 0.8, color = c_mid) + 
    labs(y = vars$variable[vari], x = "") +
    theme_minimal() +
    xlim(c(0, endtime + 0.5))
}

plot_bvar <- function(vari, df, endtime){
  ggplot(data = filter(df,var == vari),
         aes(x = time)) +
    geom_col(aes(y = value-0.5, fill = factor(value)), width = 0.8,
             alpha = 0.5) +
    geom_line(aes(y = mean_pred-0.5),
              size = 1.2, color = "white") +
    geom_line(aes(y = mean_pred-0.5),
              size = 0.8, color = "black") +
    theme_minimal() +
    theme(strip.text = element_blank(),
          panel.background = element_rect(fill = "white", color = "white"),
          legend.position = "bottom") +
    scale_y_continuous(breaks = c(-0.5,0,0.5),labels = c("0","","1"), limits = c(-0.5,0.5)) +
    scale_fill_manual(values = c(c_dark, c_dark),
                      guide = "none") +
    labs(y = vars$variable[vari], x = "") + 
    xlim(c(0, endtime+0.5))
}


plot_validation_trajectory <- function(plot_id){
  
  onept_df <- filter(bigdf, admissionid == plot_id) %>%
    group_by(time, var) %>%
    summarise(value = first(value),
              mean_pred = mean(pred),
              pred50lb = quantile(pred, 0.25),
              pred50ub = quantile(pred, 0.75),
              pred95lb = quantile(pred, 0.025),
              pred95ub = quantile(pred,0.975))
  
  endtime = max(onept_df$time)
  
  cvars <- vars[1:10,]
  cplots <- apply(select(cvars,var), 1,
                  plot_cvar, onept_df, endtime)
  
  bvars <- vars[11:16,]
  bplots <- apply(select(bvars, var), 1,
                  plot_bvar, onept_df, endtime)
  
  gg <- ggarrange(plotlist = c(cplots, bplots))
  gg
}


# interesting plot_ids 
# plot_id = 33743115  #(IMV)
# plot_id = 36924726  #(no IMV, but hosp_death)
# 38740064 pressors
# 38281218 ?palliation

plot_id <- sample(unique(bigdf$admissionid),1)
plot_validation_trajectory(plot_id)
ggsave(filename = paste0("plots/amd_validation_trajectory", plot_id,".svg"),
       height = 6, width = 8)
