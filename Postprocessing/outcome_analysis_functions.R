
make_mean_outcome_df1 <- function(outcome_df){
  outcome_df %>%
    pivot_longer(cols = imv_28:death_28,
                 names_to = "outcome",
                 values_to = "probability") %>%
    mutate(outcome = factor(outcome)) %>%
    group_by(rule, outcome, iter) %>%
    summarise(mean = mean(probability))
}

make_mean_outcome_df2 <- function(outcome_df){
  outcome_df %>%
    pivot_longer(cols = imvdeath_28:noimvsurv_28,
                 names_to = "outcome",
                 values_to = "probability") %>%
    mutate(outcome = factor(outcome)) %>%
    group_by(rule, outcome, iter) %>%
    summarise(mean = mean(probability))
}


plot_all_bars <- function(mean_outcome_df){
  
  order <- 
    mean_outcome_df %>%
    filter(outcome %in% "death_28") %>%
    summarise(m = mean(mean)) %>%
    arrange(m) %>%
    ungroup() %>%
    mutate(order = rank(m)) %>%
    select(-outcome)
  
  mean_outcome_df %>%
    filter(outcome %in% c("imvdeath_28",
                          "noimvdeath_28",
                          "imvsurv_28",
                          "noimvsurv_28")) %>%
    mutate(outcome = factor(outcome,
                            levels = c("imvdeath_28",
                                       "noimvdeath_28",
                                       "imvsurv_28",
                                       "noimvsurv_28"),
                            labels = c("IMV, death",
                                       "No IMV, death",
                                       "IMV, survival",
                                       "No IMV, survival"))) %>%
    group_by(rule, outcome) %>%
    summarise(postmean = mean(mean)) %>%
    left_join(order, by = "rule") %>%
    ggplot(aes(x = fct_reorder(rule,order),
               y = postmean,
               fill = outcome)) +
      geom_col(position = "stack", alpha = 0.8) +
    coord_flip() +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    scale_fill_manual(values = c(c_dark, c_mid, c_blue, "lightsteelblue2"),
                      name = "Outcome") +
    labs(x = "",
         y = "Mean posterior probability")
    
    
}

plot_all_amd <- function(mean_outcome_df){
  
  order <- 
    mean_outcome_df %>%
    filter(outcome %in% "death_28") %>%
    summarise(m = mean(mean)) %>%
    arrange(m) %>%
    ungroup() %>%
    mutate(order = rank(m)) %>%
    select(-outcome)
  
  
  mean_outcome_df %>%
    filter(outcome %in% c("imv_28","death_28")) %>%
    #  filter(grepl("SF",rule) | rule == "UC.1.1") %>%
    mutate(outcome = factor(outcome, 
                            levels = c("imv_obs", "death_obs","imv_28", "death_28"),
                            labels = c("IMV by day 4",
                                       "Death (before IMV) by day 4",
                                       "IMV by day 28",
                                       "Death by day 28"))) %>%
    left_join(order, by = "rule") %>%
    ggplot(aes(x = mean, 
               y = fct_reorder(rule,order), 
               fill = m)) +
    geom_density_ridges(alpha = 1, color = "grey70") +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    facet_wrap(.~outcome, scales = "free_x") +
    labs(title = "Posterior mean outcome probabilities",
         x = "Mean posterior probability",
         y = "",
         fill = "Mean mortality") +
    scale_fill_gradient2(low = c_blue, mid = "grey95", high = c_dark,
                         limits = c(0.12, 0.16), 
                         breaks = c(0.12, 0.13, 0.14,0.15, 0.16),
                         midpoint = 0.135) +
    guides(fill = guide_legend(reverse = TRUE))
  
}

plot_all <- function(mean_outcome_df){
  
  order <- 
    mean_outcome_df %>%
    filter(outcome %in% "death_28") %>%
    summarise(m = mean(mean)) %>%
    arrange(m) %>%
    ungroup() %>%
    mutate(order = rank(m)) %>%
    select(-outcome)
  
  
  mean_outcome_df %>%
    filter(outcome %in% c("imv_28","death_28")) %>%
    #  filter(grepl("SF",rule) | rule == "UC.1.1") %>%
    mutate(outcome = factor(outcome, 
                            levels = c("imv_obs", "death_obs","imv_28", "death_28"),
                            labels = c("IMV by day 4",
                                       "Death (before IMV) by day 4",
                                       "IMV by day 28",
                                       "Death by day 28"))) %>%
    left_join(order, by = "rule") %>%
    ggplot(aes(x = mean, 
               y = fct_reorder(rule,order), 
               fill = m)) +
    geom_density_ridges(alpha = 1, color = "grey70") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    facet_wrap(.~outcome, scales = "free_x") +
    labs(title = "Posterior mean outcome probabilities",
         x = "Mean posterior probability",
         y = "",
         fill = "Mean mortality") +
    scale_fill_gradient2(low = c_blue, mid = "grey95", high = c_dark,
                         limits = c(0.22, 0.26), 
                         breaks = c(0.22, 0.23,0.24, 0.25, 0.26),
                         midpoint = 0.25) +
    guides(fill = guide_legend(reverse = TRUE))
  
}

make_figure_1 <- function(sf_outcomes){
  
  sf_outcomes_mean <- 
    sf_outcomes %>%
    pivot_longer(death_28:imv_28, names_to = "outcome") %>%
    mutate(outcome = factor(outcome,
                            levels = rev(c("death_28", "imv_28")),
                            labels = rev(c("Mortality", "Invasive ventilation"
                                       ))),
           rule = factor(rule,
                         levels = c("SF.88.1",
                                    "SF.98.1",
                                    "SF.110.1"),
                         labels = c("SF < 88",
                                    "SF < 98",
                                    "SF < 110"))) %>%
    group_by(rule, iter, outcome) %>%
    summarise(post = mean(value)) %>%
    group_by(rule, outcome) %>%
    summarise(mean = mean(post),
              lb95 = quantile(post, 0.025),
              ub95 = quantile(post, 0.975)) 
  
  sf_outcomes_pt <-  sf_outcomes %>%
    select(stay_id, iter, rule, death_28, imv_28) %>%
    pivot_longer(death_28:imv_28, names_to = "outcome") %>%
    mutate(outcome = factor(outcome,
                            levels = rev(c("death_28", "imv_28")),
                            labels = rev(c("Mortality", "Invasive ventilation"
                            ))),
           rule = factor(rule,
                         levels = c("SF.88.1",
                                    "SF.98.1",
                                    "SF.110.1"),
                         labels = c("SF < 88",
                                    "SF < 98",
                                    "SF < 110"))) %>%
    group_by(stay_id, rule, outcome) %>%
    summarise(mean = mean(value))
  
  sf_outcomes_pt %>%
    ggplot(aes(x = rule, y = 100*mean)) +
    geom_line(aes(group = stay_id),
              color = c_dark, alpha = 0.05) +
    theme_minimal() +
#    geom_errorbar(data = sf_outcomes_mean,
#                aes(ymax = 100*ub95,
#                    ymin = 100*lb95,
#                    x = rep(1:3, each = 2)),
#                color = "white",
#                size = 2,
#                width = 0.1) +
#    geom_errorbar(data = sf_outcomes_mean,
#                    aes(ymax = 100*ub95,
#                        ymin = 100*lb95,
#                        x = rep(1:3, each = 2)),
#                    color = "black",
#                    size = 1,
#                  width = 0.095) +
#    geom_point(data = sf_outcomes_mean,
#                  aes(x = rep(1:3, each = 2)),
#                  color = "black",
#                  size = 2) +
    geom_ribbon(data = sf_outcomes_mean,
                aes(ymax = 100*ub95,
                    ymin = 100*lb95,
                    x = rep(1:3, each = 2)),
                fill = "white",
                alpha = 0.8) +
#        geom_line(data = sf_outcomes_mean,
#              aes(y = 100*mean, x = rep(1:3, each = 2)),
#              color = "white",
#            size = 4) +
    geom_line(data = sf_outcomes_mean,
              aes(y = 100*mean, x = rep(1:3, each = 2)),
              color = "black",
              size = 1.2) +
    facet_wrap(outcome~.) +
    labs(y = "Probability (%)",
         x = "Threshold for initiation of invasive ventilation",
         title = "Outcomes at 28 days") +
    theme(panel.grid.minor.y = element_blank(),
          axis.ticks = element_blank()) 
  
}

make_figure_1_joint <- function(sf_outcomes){
  
  sf_outcomes_mean <- 
    sf_outcomes %>%
    select(cohort, stay_id, iter, rule, imv_28, death_28) %>%
    pivot_longer(death_28:imv_28, names_to = "outcome") %>%
    mutate(outcome = factor(outcome,
                            levels = rev(c("death_28", "imv_28")),
                            labels = rev(c("Mortality", "Invasive ventilation"
                            ))),
           rule = factor(rule,
                         levels = c("SF.88.1",
                                    "SF.98.1",
                                    "SF.110.1"),
                         labels = c("SF < 88",
                                    "SF < 98",
                                    "SF < 110")),
           cohort = factor(cohort,
                           levels = c("MIMIC",
                                      "AmsterdamUMCdb"),
                           labels = c("MIMIC-IV",
                                      "AmsterdamUMCdb"))) %>%
    group_by(cohort, rule, iter, outcome) %>%
    summarise(post = mean(value)) %>%
    group_by(cohort, rule, outcome) %>%
    summarise(mean = mean(post),
              lb95 = quantile(post, 0.025),
              ub95 = quantile(post, 0.975)) 
  
  sf_outcomes_pt <-  sf_outcomes %>%
    select(cohort, stay_id, iter, rule, death_28, imv_28) %>%
    pivot_longer(death_28:imv_28, names_to = "outcome") %>%
    mutate(outcome = factor(outcome,
                            levels = rev(c("death_28", "imv_28")),
                            labels = rev(c("Mortality", "Invasive ventilation"
                            ))),
           rule = factor(rule,
                         levels = c("SF.88.1",
                                    "SF.98.1",
                                    "SF.110.1"),
                         labels = c("SF < 88",
                                    "SF < 98",
                                    "SF < 110")),
           cohort = factor(cohort,
                           levels = c("MIMIC",
                                      "AmsterdamUMCdb"),
                           labels = c("MIMIC-IV",
                                      "AmsterdamUMCdb"))) %>%
    group_by(cohort, stay_id, rule, outcome) %>%
    summarise(mean = mean(value))
  
  sf_outcomes_pt %>%
    ggplot(aes(x = rule, y = 100*mean)) +
    geom_line(aes(group = stay_id),
              color = c_dark, alpha = 0.05) +
    theme_minimal() +
    #    geom_errorbar(data = sf_outcomes_mean,
    #                aes(ymax = 100*ub95,
    #                    ymin = 100*lb95,
    #                    x = rep(1:3, each = 2)),
    #                color = "white",
    #                size = 2,
    #                width = 0.1) +
    #    geom_errorbar(data = sf_outcomes_mean,
    #                    aes(ymax = 100*ub95,
    #                        ymin = 100*lb95,
    #                        x = rep(1:3, each = 2)),
  #                    color = "black",
  #                    size = 1,
  #                  width = 0.095) +
  #    geom_point(data = sf_outcomes_mean,
  #                  aes(x = rep(1:3, each = 2)),
  #                  color = "black",
  #                  size = 2) +
  geom_ribbon(data = sf_outcomes_mean,
              aes(ymax = 100*ub95,
                  ymin = 100*lb95,
                  x = rep(1:3, each = 2, times = 2)),
              fill = "white",
              alpha = 0.8) +
    #        geom_line(data = sf_outcomes_mean,
    #              aes(y = 100*mean, x = rep(1:3, each = 2)),
    #              color = "white",
    #            size = 4) +
    geom_line(data = sf_outcomes_mean,
              aes(y = 100*mean, x = rep(1:3, each = 2, times = 2)),
              color = "black",
              size = 1.2) +
    facet_wrap(outcome~cohort,
               strip.position = "top") +
    labs(y = "Probability (%)",
         x = "Threshold for initiation of invasive ventilation",
         title = "") +
    theme(panel.grid.minor.y = element_blank(),
          axis.ticks = element_blank()) 
  
}

make_table_1 <- function(mean_outcome_df){
  
  mean_outcome_df %>%
    filter(outcome %in% c("imv_28","death_28")) %>%
    group_by(rule, outcome) %>%
    summarise(post = mean(mean),
              lb95 = quantile(mean, 0.025),
              ub95 = quantile(mean, 0.975)) %>%
    mutate(rule = factor(rule,
                         levels = c(
                           "SF.88.1",
                           "SF.98.1",
                           "SF.110.1",
                           "UC.1.1",
                           "SFRR.25.1",
                           "SFRR.35.1",
                           "SFWoB.98.1",
                           "SF.98.2",
                           "SF.98.3",
                           "Delta.0.5.1",
                           "Delta.1.1",
                           "MultiOrgan_Or.1.1",
                           "MultiOrgan_And.1.1"),
                         labels = c(
                           "SF < 88",
                           "SF < 98",
                           "SF < 110",
                           "Usual care",
                           "SF < 98 and RR > 25",
                           "SF < 98 and RR > 35",
                           "SF < 98 and abnormal work of breathing",
                           "SF < 98 for two consecutive hours",
                           "SF < 98 for four consecutive hours",
                           "SF predicted to be less than 88 in 30 minutes",
                           "SF predicted to be less than 88 in 60 minutes",
                           "Respiratory, hemodynamic, or neurologic trial criteria",
                           "Respiratory and either hemodynamic or neurologic trial criteria"
                         ),
                         ordered = T)) %>%
    mutate(label = paste0(round(100*post, 1), " (",
                          round(100*lb95, 1), " to ",
                          round(100*ub95, 1), ")")) %>%
    select(rule, outcome, label) %>%
    pivot_wider(names_from = outcome, 
                values_from = label) %>%
    arrange(rule)
}

odds <- function(x){x/(1-x)}

calculate_ORs <- function(rule1,
                          mean_outcome_df, 
                          outcome1, 
                          rule2 = "UC.1.1",
                          MCID1 = 0.01,
                          MCID2 = 0.03, 
                          ORMCID1 = 0.9,
                          ORMCID2 = 0.8){
  temp <- mean_outcome_df %>%
    filter(rule %in% c(rule1, rule2), outcome == outcome1) %>%
    pivot_wider(names_from = rule, values_from = mean) %>%
    ungroup() %>%
    select(-outcome, -iter)
  
  temp2 <- unlist(odds(temp[,1])/odds(temp[,2]))
  
  temp3 <- unlist(temp[,2] - temp[,1])
  
  c(meanOR = mean(temp2), 
    lb95OR = quantile(temp2, 0.025), 
    ub95OR = quantile(temp2, 0.975),
    benefit = mean(temp3 > 0),
    probMCID1 = mean(temp3 > MCID1), 
    probMCID2 = mean(temp3 > MCID2),
    probORMCID1 = mean(temp2 < ORMCID1),
    probORMCID2 = mean(temp2 < ORMCID2))
}

calculate_RRs <- function(rule1,
                          mean_outcome_df, 
                          outcome1, 
                          rule2 = "UC.1.1",
                          MCID1 = 0.01,
                          MCID2 = 0.03, 
                          RRMCID1 = 0.9,
                          RRMCID2 = 0.8){
  temp <- mean_outcome_df %>%
    filter(rule %in% c(rule1, rule2), outcome == outcome1) %>%
    pivot_wider(names_from = rule, values_from = mean) %>%
    ungroup() %>%
    select(-outcome, -iter)
  
  temp2 <- unlist(temp[,1]/temp[,2])
  
  temp3 <- unlist(temp[,2] - temp[,1])
  
  c(meanRR = mean(temp2), 
    lb95RR = quantile(temp2, 0.025), 
    ub95RR = quantile(temp2, 0.975),
    benefit = mean(temp3 > 0),
    probMCID1 = mean(temp3 > MCID1), 
    probMCID2 = mean(temp3 > MCID2),
    probORMCID1 = mean(temp2 < RRMCID1),
    probORMCID2 = mean(temp2 < RRMCID2))
}

calculate_ARR <- function(rule1,
                          mean_outcome_df, 
                          outcome1, 
                          rule2 = "UC.1.1",
                          MCID1 = 0.01,
                          MCID2 = 0.03){
  temp <- mean_outcome_df %>%
    filter(rule %in% c(rule1, rule2), outcome == outcome1) %>%
    pivot_wider(names_from = rule, values_from = mean) %>%
    relocate(iter, outcome, rule1, rule2) %>%
      ungroup() %>%
    select(-outcome, -iter)
  
  temp3 <- unlist(temp[,2] - temp[,1])
  
  c(meanARR = mean(temp3), 
    lb95ARR = quantile(temp3, 0.025), 
    ub95ARR = quantile(temp3, 0.975),
    benefit = mean(temp3 > 0),
    probMCID1 = mean(temp3 > MCID1), 
    probMCID2 = mean(temp3 > MCID2))
}

calculate_RRR <- function(rule1,
                          mean_outcome_df, 
                          outcome1, 
                          rule2 = "UC.1.1"){
  temp <- mean_outcome_df %>%
    filter(rule %in% c(rule1, rule2), outcome == outcome1) %>%
    pivot_wider(names_from = rule, values_from = mean) %>%
    relocate(iter, outcome, rule1, rule2) %>%
    ungroup() %>%
    select(-outcome, -iter)
  
  temp3 <- unlist(temp[,2]/temp[,1])
  
  c(meanRRR = mean(temp3), 
    lb95RRR = quantile(temp3, 0.025), 
    ub95RRR = quantile(temp3, 0.975))
}

eval <- function(RR){RR + sqrt(RR*(RR-1))}
