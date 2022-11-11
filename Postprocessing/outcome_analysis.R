# IMV outcomes analysis file

setwd("C:/Git/PAHRC/imvThresholdsTargetTrial")
library(tidyverse)
library(kableExtra)
library(ggridges)
library(ggpubr)

# colours
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")
c_blue = "cornflower blue"

source("outcome_analysis_functions.R")

pth <- "C:/Users/chris/OneDrive/Documents/LargeDataFiles/mimic_predictedOutcomes"
pth2 <- "C:/Users/chris/OneDrive/Documents/LargeDataFiles/mimic_predictedOutcomes1-200"
files <- list.files(path = pth,
                    full.names = T)

files2 <- list.files(path = pth2,
                     full.names = T)

ids <- gsub("outcomes", "", list.files(path = pth))
ids <- as.numeric(gsub(".rds", "", ids))

ids2 <- gsub("outcomes", "", list.files(path = pth2))
ids2 <- as.numeric(gsub(".rds", "", ids2))

outcome_df <- lapply(files, readRDS) 
names(outcome_df) <- ids
outcome_df <- bind_rows(outcome_df, .id = "stay_id")

outcome_df2 <- lapply(files2, readRDS) 
names(outcome_df2) <- ids2
outcome_df2 <- bind_rows(outcome_df2, .id = "stay_id")
outcome_df2 <- mutate(outcome_df2, iter = iter + 200)

outcome_df1 <- outcome_df
outcome_df <- bind_rows(outcome_df1, outcome_df2)
rm(outcome_df1)
rm(outcome_df2)
gc()

mean_outcome_df <- make_mean_outcome_df1(outcome_df)

saveRDS(mean_outcome_df, "mimic_mean_outcome_df.rds")

mean_outcome_df <- readRDS("mimic_mean_outcome_df.rds")

# ARR

mean_outcome_df %>%
  filter(rule %in% c("SF.88.1",
                     "SF.98.1",
                     "SF.110.1")) %>%
  pivot_wider(names_from = rule,
              values_from = mean) %>%
  mutate(delta110v88 = SF.110.1 - SF.88.1,
         delta98v88 = SF.98.1 - SF.88.1) %>%
  group_by(outcome) %>%
  summarise(delta110v88.mean = round(100*mean(delta110v88),1),
            delta110v88.lb95 = round(100*quantile(delta110v88,0.025),1),
            delta110v88.ub95 = round(100*quantile(delta110v88,0.975),1),
            delta98v88.mean = round(100*mean(delta98v88),1),
            delta98v88.lb95 = round(100*quantile(delta98v88,0.025),1),
            delta98v88.ub95 = round(100*quantile(delta98v88,0.975),1)) %>%
  pivot_longer(delta110v88.mean:delta98v88.ub95,
               names_to = c("contrast","quantity"),
               names_pattern = "([[:alnum:]]+).([[:alnum:]]+)") %>%
  pivot_wider(names_from = quantity,
              values_from = value)

plot_all(mean_outcome_df)
ggsave("plots/mimic_allthresholds.svg", width = 10, height = 7)
    
plot_all_bars(mean_outcome_df)
ggsave("plots/mimic_allthresholds_bars.svg", height = 8, width = 8)


  # SF ratio thresholds
  
  sf_outcomes <- filter(outcome_df, rule %in% c("SF.88.1",
                                                "SF.98.1",
                                                "SF.110.1"))

  # patients intubated after the 96h period:
  
mean95cri <- function(x){paste0(round(mean(x)*100, 1),
                                "% (CrI ",
                                round(quantile(x,0.025)*100, 1),
                                " to ",
                                round(quantile(x,0.975)*100, 1),
                                ")")}
  
sf_outcomes %>%
    ungroup() %>%
    group_by(iter,rule) %>%
    select(imv_obs, imv_28) %>%
    mutate(imv_after = imv_28-imv_obs) %>%
    summarise_all(mean) %>%
    ungroup() %>%
    group_by(rule) %>%
    summarise(imv_obs_label = mean95cri(imv_obs),
              imv_28_label = mean95cri(imv_28),
              imv_after_label = mean95cri(imv_after)) %>%
  write_csv(file = "imv_sfoutcomes.csv")

    
  # simple Figure 1: only SF outcomes
  
  make_figure_1(sf_outcomes)
ggsave("plots/figure1.svg", width = 8, height = 8, dpi = 300)

# joint figure 1 between MIMIC and AmsterdamUMCdb
saveRDS(amd_sf, "amd_sf.rds")
amd_sf <- readRDS("amd_sf.rds") %>%
  ungroup() %>%
  select(-admissionid)

joint_sf <- bind_rows(MIMIC = sf_outcomes,
                      AmsterdamUMCdb = amd_sf,
                      .id = "cohort")

make_figure_1_joint(joint_sf)
ggsave("plots/figure_1_joint.svg", width = 8, height = 8, dpi = 300)


  # Table 2 : primary outcomes and secondary outcomes
make_table_1(mean_outcome_df) %>%
  write.csv("table2.csv")

  # subgroups
  pdx_b <- readRDS("pdx_b.rds")
  
  female <- filter(pdx_b, gender == "F")$stay_id
  male  <- filter(pdx_b, gender == "M")$stay_id
  
  age50 <- filter(pdx_b, anchor_age <= 50)$stay_id
  age50to60 <- filter(pdx_b, anchor_age > 50, anchor_age <= 60)$stay_id
  age60to70 <- filter(pdx_b, anchor_age > 60, anchor_age <= 70)$stay_id
  age70to80 <- filter(pdx_b, anchor_age > 70, anchor_age <= 80)$stay_id
  age80plus <- filter(pdx_b, anchor_age > 80)$stay_id
  
  weight60kg <- filter(pdx_b, weight_admit < 60)$stay_id
  weight60to100kg <- filter(pdx_b, weight_admit >= 60, weight_admit < 100)$stay_id
  weight100plus <- filter(pdx_b, weight_admit >= 100)$stay_id
  
  white <- filter(pdx_b, ethnicity == "WHITE")$stay_id
  black <- filter(pdx_b, ethnicity == "BLACK/AFRICAN AMERICAN")$stay_id
  asian <- filter(pdx_b, ethnicity == "ASIAN")$stay_id
  hispanic <- filter(pdx_b, ethnicity == "HISPANIC/LATINO")$stay_id
    
  fio2_80plus <- filter(pdx_b, fio2_bl >= 80)$stay_id
  fio2_60to80 <- filter(pdx_b, fio2_bl >= 60, fio2_bl < 80)$stay_id
  fio2_40to80 <- filter(pdx_b, fio2_bl >= 40, fio2_bl < 60)$stay_id
  
  hfnc_bl <- filter(pdx_b, hfnc_bl == 1)$stay_id
  niv_bl  <- filter(pdx_b, niv_bl == 1)$stay_id
  nrb_bl  <- filter(pdx_b, nrb_bl == 1)$stay_id
  
  year2008.2010 <- filter(pdx_b, anchor_year_group == "2008 - 2010")
  year2011.2013 <- filter(pdx_b, anchor_year_group == "2011 - 2013")
  year2014.2016 <- filter(pdx_b, anchor_year_group == "2014 - 2016")
  year2017.2019 <- filter(pdx_b, anchor_year_group == "2017 - 2019")
    
  subgroup_posterior <- function(df, ids){
    filter(df, stay_id %in% ids) %>%
    select(stay_id, iter, rule, imv_28, death_28) %>%  
    group_by(rule, iter) %>%
    summarise(death28 = mean(death_28),
              imv28 = mean(imv_28))
  }

  # slow    
outcomes_by_age <- bind_rows(
  subgroup_posterior(sf_outcomes, age50),
  subgroup_posterior(sf_outcomes, age50to60),
  subgroup_posterior(sf_outcomes, age60to70),
  subgroup_posterior(sf_outcomes, age70to80),
  subgroup_posterior(sf_outcomes, age80plus),
  .id = "category")

outcomes_by_age <- mutate(outcomes_by_age,
       category = case_when(
         category == 1 ~ "Age less than 50",
         category == 2 ~ "Age 50 to 60",
         category == 3 ~ "Age 60 to 70",
         category == 4 ~ "Age 70 to 80",
         category == 5 ~ "Age 80 or more"
       ))

saveRDS(outcomes_by_age, "outcomes_by_age.rds")

# slow    
outcomes_by_sex <- bind_rows(
  subgroup_posterior(sf_outcomes, female),
  subgroup_posterior(sf_outcomes, male),
  .id = "category")

outcomes_by_sex <- mutate(outcomes_by_sex,
                          category = case_when(
                            category == 1 ~ "Female sex",
                            category == 2 ~ "Male sex"
                          ))

saveRDS(outcomes_by_sex, "outcomes_by_sex.rds")

# slow    
outcomes_by_race <- bind_rows(
  subgroup_posterior(sf_outcomes, white),
  subgroup_posterior(sf_outcomes, black),
  subgroup_posterior(sf_outcomes, asian),
  subgroup_posterior(sf_outcomes, hispanic),
  .id = "category")

outcomes_by_race <- mutate(outcomes_by_race,
                          category = case_when(
                            category == 1 ~ "Race/ethnicity: White",
                            category == 2 ~ "Race/ethnicity: Black",
                            category == 3 ~ "Race/ethnicity: Asian",
                            category == 4 ~ "Race/ethnicity: Hispanic"))

saveRDS(outcomes_by_race, "outcomes_by_race.rds")

# slow    
outcomes_by_weight <- bind_rows(
  subgroup_posterior(sf_outcomes, weight60kg),
  subgroup_posterior(sf_outcomes, weight60to100kg),
  subgroup_posterior(sf_outcomes, weight100plus),
  .id = "category") %>% 
  mutate(category = case_when(
           category == 1 ~ "Weight less than 60kg",
           category == 2 ~ "Weight 60kg to 100kg",
           category == 3 ~ "Weight 100kg or more"))

saveRDS(outcomes_by_weight, "outcomes_by_weight.rds")

# slow    
outcomes_by_o2dev <- bind_rows(
  subgroup_posterior(sf_outcomes, hfnc_bl),
  subgroup_posterior(sf_outcomes, niv_bl),
  subgroup_posterior(sf_outcomes, nrb_bl),
  .id = "category") %>% 
  mutate(category = case_when(
           category == 1 ~ "HFNC at eligibility",
           category == 2 ~ "NIV at eligibility",
           category == 3 ~ "NRB at eligibility"))
saveRDS(outcomes_by_o2dev, "outcomes_by_o2dev.rds")

# slow    
outcomes_by_fio2 <- bind_rows(
  subgroup_posterior(sf_outcomes, fio2_40to80),
  subgroup_posterior(sf_outcomes, fio2_60to80),
  subgroup_posterior(sf_outcomes, fio2_80plus),
  .id = "category") %>% 
  mutate(category = case_when(
           category == 1 ~ "0.4 <= Baseline FiO2 < 0.6",
           category == 2 ~ "0.6 <= Baseline FiO2 < 0.8",
           category == 3 ~ "0.8 <= Baseline FiO2 < 1.0"))
saveRDS(outcomes_by_fio2, "outcomes_by_fio2.rds")

outcomes_by_year <- bind_rows(
  subgroup_posterior(sf_outcomes, year2008.2010),
  subgroup_posterior(sf_outcomes, year2011.2013),
  subgroup_posterior(sf_outcomes, year2014.2016),
  subgroup_posterior(sf_outcomes, year2017.2019),
  .id = "category") %>% 
  mutate(category = case_when(
    category == 1 ~ "2008-2010",
    category == 2 ~ "2011-2013",
    category == 3 ~ "2014-2016",
    category == 3 ~ "2017-2019"))
saveRDS(outcomes_by_year, "outcomes_by_year.rds")


subgroup_outcomes <- bind_rows(readRDS("outcomes_by_age.rds"),
                               readRDS("outcomes_by_sex.rds"),
                               readRDS("outcomes_by_race.rds"),
                               readRDS("outcomes_by_weight.rds"),
                               readRDS("outcomes_by_fio2.rds"),
                               readRDS("outcomes_by_o2dev.rds"))

subgroup_outcomes %>%
  pivot_longer(death28:imv28, names_to = "outcome") %>%
  mutate(category = factor(category,
                           levels = c("NRB at eligibility",
                                      "NIV at eligibility",
                                      "HFNC at eligibility",
                                      "0.8 <= Baseline FiO2 < 1.0",
                                      "0.6 <= Baseline FiO2 < 0.8",
                                      "0.4 <= Baseline FiO2 < 0.6",
                                      "Weight 100kg or more",
                                      "Weight 60kg to 100kg",
                                      "Weight less than 60kg",
                                      "Race/ethnicity: Hispanic",
                                      "Race/ethnicity: Asian",
                                      "Race/ethnicity: Black",
                                      "Race/ethnicity: White",
                                      "Male sex",
                                      "Female sex",
                                      "Age 80 or more",
                                      "Age 70 to 80",
                                      "Age 60 to 70",
                                      "Age 50 to 60",
                                      "Age less than 50"))) %>%
  mutate(outcome = factor(outcome,
                          levels = c("imv28", "death28"),
                          labels = c("Invasive ventilation (%)",
                                     "Death (%)")),
         rule = factor(rule,
                       levels = c("SF.88.1",
                                  "SF.98.1",
                                  "SF.110.1"),
                       labels = c("SF < 88",
                                  "SF < 98",
                                  "SF < 110"))) %>%
  ggplot(aes(x = 100*value, fill = rule, y = category)) +
  geom_density_ridges(scale = 1, color = NA, alpha = 0.8) +
  facet_wrap(.~outcome, scales = "free_x",
             strip.position = "bottom") + 
  theme_minimal() +
  scale_fill_manual(values = c(c_light, c_mid, c_dark),
                    name = "Threshold") +
  theme(panel.grid.minor.x = element_blank(),
        panel.spacing = unit(2, "lines")) +
  labs(y = "", x = "") +
  theme(
    strip.background = element_blank(),
    strip.placement = "outside"
  )

ggsave("plots/figure2.svg", width = 10, height = 8, dpi = 300)

# by concept
  
wobSF98 <- 
  mean_outcome_df %>%
  filter(rule == "SF.98.1") %>%
  mutate(concept = "Work of breathing")

durSF98 <- 
  mean_outcome_df %>%
  filter(rule == "SF.98.1") %>%
  mutate(concept = "Duration")

mean_outcome_df %>%
  mutate(concept = case_when(
    rule %in% c("SF.98.1","SF.88.1","SF.110.1") ~ "Hypoxemia",
    grepl("SFRR", rule) ~ "Tachypnea",
    rule %in% c("SF.98.2", "SF.98.3") ~ "Duration",
    grepl("WoB", rule) ~ "Work of breathing",
    grepl("Delta", rule) ~ "Trajectory",
    grepl("Multi", rule) ~ "Multi-organ involvement",
    grepl("UC.", rule) ~ "Usual care"
  )) %>%
  bind_rows(wobSF98) %>%
  bind_rows(durSF98) %>%
  filter(outcome %in% c("imv_28","death_28")) %>%
  #  filter(grepl("SF",rule) | rule == "UC.1.1") %>%
  mutate(outcome = factor(outcome, 
                          levels = c("imv_obs", "death_obs","imv_28", "death_28"),
                          labels = c("IMV by day 4",
                                     "Death (before IMV) by day 4",
                                     "IMV by day 28",
                                     "Death by day 28"))) %>%
  left_join(order, by = "rule") %>%
  mutate(rule = case_when(
    rule == "SF.98.1" & concept == "Hypoxemia" ~ "SF < 98",
    rule == "SF.98.1" & concept == "Work of breathing" ~ "SF < 98 only",
    rule == "SF.98.1" & concept == "Duration" ~ "SF < 98 once",
    rule == "SF.88.1" ~ "SF < 88",
    rule == "SF.110.1" ~ "SF < 110",
    rule == "SF.98.2" ~ "SF < 98 for two hours",
    rule == "SF.98.3" ~ "SF < 98 for four hours",
    rule == "SFWoB.98.1" ~ "SF < 98 and abnormal work of breathing",
    rule == "MultiOrgan_Or.1.1" ~ "Respiratory, hemodynamic, or neurologic criteria",
    rule == "MultiOrgan_And.1.1" ~ "Respiratory and either hemodynamic or neurologic criteria",
    rule == "Delta.1.1" ~ "Predicted SF < 88 in next 60 minutes",
    rule == "Delta.0.5.1" ~ "Predicted SF < 88 in next 30 minutes",
    rule == "UC.1.1" ~ "Usual care",
    rule == "SFRR.25.1" ~ "SF < 98 and RR > 25",
    rule == "SFRR.35.1" ~ "SF < 98 and RR > 35"
  )) %>%
  mutate(rule = factor(
    rule,
    levels = rev(c(
      "SF < 110","SF < 98","SF < 88",
      "SF < 98 and RR > 25","SF < 98 and RR > 35",
      "SF < 98 only","SF < 98 and abnormal work of breathing",
      "SF < 98 once","SF < 98 for two hours","SF < 98 for four hours",
      "Respiratory, hemodynamic, or neurologic criteria","Respiratory and either hemodynamic or neurologic criteria",
      "Predicted SF < 88 in next 60 minutes","Predicted SF < 88 in next 30 minutes",
      "Usual care")))) %>%
  mutate(concept = factor(concept,
                          levels = c(
                            "Hypoxemia",
                            "Tachypnea",
                            "Work of breathing",
                            "Duration",
                            "Trajectory",
                            "Multi-organ involvement",
                            "Usual care"
                          ))) %>%
  ggplot(aes(x = mean, 
             y = rule, 
             fill = m)) +
  geom_density_ridges(alpha = 1) +
  theme_minimal() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "black")) +
  facet_grid(concept~outcome, scales = "free",
             space = "free_y") +
  labs(title = "Posterior mean outcome probabilities",
       x = "Mean posterior probability",
       y = "",
       fill = "Mean mortality") +
  scale_fill_gradient2(low = c_blue, mid = "grey95", high = c_dark,
                       limits = c(0.21, 0.265), 
                       breaks = c(0.21,0.22, 0.23,0.24, 0.25, 0.26),
                       midpoint = 0.235) +
  theme(panel.spacing.x = unit(2, "lines")) +
  guides(fill = guide_legend(reverse = TRUE))
ggsave("plots/mimic_outcomes_concept.svg")
  
  
mean_outcome_df %>%
    filter(outcome %in% c("death_28","imv_28","death_obs")) %>%
    pivot_wider(names_from = rule,
                values_from = mean) %>%
    mutate(DMB = DeMontBauer.1.1-DeMontBauer.1.2,
           FLO = FLORALI.1.1-FLORALI.1.2,
           SF.1 = SF.120.1-SF.90.1,
           SF.2 = SF.120.2-SF.90.2,
           ROX.1 = ROX.4.1-ROX.3.1,
           ROX.2 = ROX.5.2-ROX.4.2,
           VP = Vasopressor.1.1-Vasopressor.1.2) %>%
    select(-c(DeMontBauer.1.1:Vasopressor.1.2)) %>%
    pivot_longer(DMB:VP, names_to = "Rule") %>%
    ggplot(aes(x = value, y = Rule, fill = outcome)) +
    geom_density_ridges()
  

# table 2
  
mean_outcome_df %>%
  filter(outcome != "Observation time") %>%
  group_by(outcome, rule) %>%
  summarise(posterior_mean = round(100*mean(mean),1),
            CI95lb = round(quantile(100*mean, 0.025),1),
            CI95ub = round(quantile(100*mean, 0.975),1)) %>%
  kable() %>%
  kable_minimal(lightable_options = "striped")

# patient-level

pt_level_df <- outcome_df %>%
  group_by(rule, iter, stay_id) %>% 
  select(imv_28, death_28) %>%
  group_by(rule, stay_id) %>%
  summarise(pmort = mean(death_28),
            pimv = mean(imv_28)) 

N = length(unique(outcome_df$stay_id))

ptlevel_rankplot <- function(pt_level_df){

pt_level_df %>%
  select(-pimv) %>%
  ungroup() %>%
  group_by(stay_id) %>%
  mutate(rank = rank(pmort)) %>%
  ggplot(aes(x = rank, fill = reorder(rule,rank))) +
  geom_bar(position = "stack", alpha = 0.5, col = "black", width = 1) +
  theme_minimal()
}

ptlevel_rankplot(pt_level_df)


highfio2 <- filter(pdx_b, fio2_bl > 80)
ptlevel_rankplot(filter(pt_level_df, 
                        stay_id %in% highfio2$stay_id))

hfnc_bl <- filter(pdx_b, hfnc_bl == 1)
ptlevel_rankplot(filter(pt_level_df, 
                        stay_id %in% hfnc_bl$stay_id))

women <- filter(pdx_b, gender == "F")
ptlevel_rankplot(filter(pt_level_df, 
                        stay_id %in% women$stay_id))

nonwhite <- filter(pdx_b, ethnicity != "White")
ptlevel_rankplot(filter(pt_level_df, 
                        stay_id %in% nonwhite$stay_id))

pressor <- filter(pdx_b, pressor_bl == 1)
ptlevel_rankplot(filter(pt_level_df, 
                        stay_id %in% pressor$stay_id))

black <- filter(pdx_b, ethnicity == "BLACK/AFRICAN AMERICAN")
ptlevel_rankplot(filter(pt_level_df,
                        stay_id %in% black$stay_id))

pt_level_df %>%
  select(-pimv) %>%
  ungroup() %>%
  group_by(stay_id) %>%
  mutate(rank = rank(pmort)) %>%
  filter(rank == 1) %>%
  ungroup() %>%
  summarise(mean(pmort))

pt_level_df %>%
  ggplot(aes(x = rule, y = pmort, group = stay_id)) +
  geom_line(color = c_dark, alpha = 0.1) +
  theme_minimal() + 
  coord_flip()



outcome_df %>%
  group_by(rule, iter, stay_id) %>% 
  select(imv_28, death_28) %>%
  group_by(rule, stay_id) %>%
  summarise(meansurv = 1-mean(death_28),
            meanimv = mean(imv_28)) %>%
#  pivot_longer(cols = meansurv:meanimv,
#               names_to = "outcome",
#               values_to = "value") %>%
  ggplot(aes(x = 100*meanimv,
             y = 100*meansurv)) +
  geom_point(alpha = 0.4) + 
  facet_wrap(.~rule) +
  theme_minimal()


# Table 3 

rules = unique(mean_outcome_df$rule)

tbl3_rules <- rules[c(5,7,6,
                      10, 11,
                      12, 
                      8, 9,
                      2, 1,
                      4,3)]

tbl3_rulenames = c(
  "Rule",
  "SF < 110",
  "SF < 98",
  "SF < 88",
  "SF < 98 and RR > 25",
  "SF < 98 and RR > 35",
  "SF < 98 and abnormal work of breathing",
  "SF < 98 for 2 hours",
  "SF < 98 for 4 hours",
  "Predicted SF < 88 in 60 minutes",
  "Predicted SF < 88 in 30 minutes",
  "Respiratory, hemodynamic, or neurologic trial criteria",
  "Respiratory and hemodynamic or neurologic trial criteria"
)

tbl3 <- bind_rows(lapply(tbl3_rules, calculate_ORs,
                  mean_outcome_df, "death_28"))


names(tbl3)[2] <- "lb95OR"
names(tbl3)[3] <- "ub95OR"

tbl3 %>%
  mutate(rule = tbl3_rules) %>%
  saveRDS("mimic_tbl3.rds")

textsize = 10

ggtext_size <- function(base_size, ratio = 0.8) {
  ratio * base_size / ggplot2::.pt
}


tbl3 <- tbl3 %>%
  mutate(label = paste0(
    round(meanOR, 2), " (",
    round(lb95OR,2), " to ",
    round(ub95OR,2), ")")) %>%
  mutate(benefit = as.character(round(benefit, 2)),
         probMCID1 = as.character(round(probMCID1, 2)),
         probMCID2 = as.character(round(probMCID2, 2)),
         probORMCID1 = as.character(round(probORMCID1, 2)),
         probORMCID2 = as.character(round(probORMCID2, 2)))

gg1 <- bind_rows(data.frame(
    meanOR = NA,
    lb95OR = NA,
    ub95OR = NA,
    benefit = "P(OR < 1.0)",
    probMCID1 = NA,
    probMCID2 = NA,
    probORMCID1 = "P(OR < 0.9)",
    probORMCID2 = "P(OR < 0.8)",
    label = "Odds Ratio"),
    tbl3) %>%
  mutate(rule = factor(tbl3_rulenames, 
                     levels = rev(tbl3_rulenames),
                     ordered = T)) %>%
  mutate(bold = ifelse(rule == "Rule", "bold", "plain")) %>%
  ggplot(aes(y = meanOR,
             x = fct_reorder(rule, -meanOR))) +
  geom_hline(yintercept = 1, color = "gray80", size = 1) +
  geom_pointrange(aes(ymin = lb95OR,
                      ymax = ub95OR)) +
  coord_flip() +
  theme_minimal(base_size = textsize) +
  labs(y = "Posterior odds ratio (95% credible interval)",
       x = "",
       title = "MIMIC-IV") +
  scale_y_continuous(trans = "log",
                     breaks = c(0.8, 0.9, 1.0, 1.1),
                     limits = c(0.77, 2)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_text(aes(label = label, y = 1.11, fontface = bold), hjust = 0,
            size = ggtext_size(textsize)) +
  geom_text(aes(label = benefit, y = 1.5, fontface = bold), hjust = 1,
            size = ggtext_size(textsize)) + 
  geom_text(aes(label = probORMCID1, y = 1.75, fontface = bold), hjust = 1,
            size = ggtext_size(textsize)) +
  geom_text(aes(label = probORMCID2, y = 2, fontface = bold), hjust = 1,
            size = ggtext_size(textsize)) +
  theme(axis.text.y = element_text(face = c(rep('plain', 12),"bold")),
        axis.title.x = element_text(hjust = 0),
        title = element_text(hjust = 0))
  
  ggsave(filename = "plots/figure3.svg", width = 10, height = 4)

# Amsterdam version
  
  amd_mean_outcome_df <- readRDS("amd_mean_outcome_df.rds")
  
  rules = unique(amd_mean_outcome_df$rule)
  
  tbl3_rules <- rules[c(5,7,6,
                        10, 11,
                        8, 9,
                        2, 1,
                        4,3)]
  
  tbl3_rulenames = c(
    "Rule",
    "SF < 110",
    "SF < 98",
    "SF < 88",
    "SF < 98 and RR > 25",
    "SF < 98 and RR > 35",
    "SF < 98 for 2 hours",
    "SF < 98 for 4 hours",
    "Predicted SF < 88 in 60 minutes",
    "Predicted SF < 88 in 30 minutes",
    "Respiratory, hemodynamic, or neurologic trial criteria",
    "Respiratory and hemodynamic or neurologic trial criteria"
  )
  
  tbl3 <- bind_rows(lapply(tbl3_rules, calculate_ORs,
                           amd_mean_outcome_df, "death_28"))
  
  
  names(tbl3)[2] <- "lb95OR"
  names(tbl3)[3] <- "ub95OR"
  
  tbl3 %>%
    mutate(rule = tbl3_rules) %>%
    saveRDS("amds_tbl3.rds")
  
  
  textsize = 10
  
  ggtext_size <- function(base_size, ratio = 0.8) {
    ratio * base_size / ggplot2::.pt
  }
  
  
  tbl3 <- tbl3 %>%
    mutate(label = paste0(
      round(meanOR, 2), " (",
      round(lb95OR,2), " to ",
      round(ub95OR,2), ")")) %>%
    mutate(benefit = as.character(round(benefit, 2)),
           probMCID1 = as.character(round(probMCID1, 2)),
           probMCID2 = as.character(round(probMCID2, 2)),
           probORMCID1 = as.character(round(probORMCID1, 2)),
           probORMCID2 = as.character(round(probORMCID2, 2)))
  
  gg2<- bind_rows(data.frame(
    meanOR = NA,
    lb95OR = NA,
    ub95OR = NA,
    benefit = "P(OR < 1.0)",
    probMCID1 = NA,
    probMCID2 = NA,
    probORMCID1 = "P(OR < 0.9)",
    probORMCID2 = "P(OR < 0.8)",
    label = "Odds Ratio (OR)"),
    tbl3) %>%
    mutate(rule = factor(tbl3_rulenames, 
                         levels = rev(tbl3_rulenames),
                         ordered = T)) %>%
    mutate(bold = ifelse(rule == "Rule", "bold", "plain")) %>%
    ggplot(aes(y = meanOR,
               x = fct_reorder(rule, -meanOR))) +
    geom_hline(yintercept = 1, color = "gray80", size = 1) +
    geom_pointrange(aes(ymin = lb95OR,
                        ymax = ub95OR)) +
    coord_flip() +
    theme_minimal(base_size = textsize) +
    labs(y = "Posterior odds ratio (95% credible interval)",
         x = "", 
         title = "AmsterdamUMCdb") +
    scale_y_continuous(trans = "log",
                       breaks = c(0.8, 0.9, 1.0, 1.1, 1.2),
                       limits = c(0.8, 2.8)) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank()) +
    geom_text(aes(label = label, y = 1.4), hjust = 0,
              size = ggtext_size(textsize)) +
    geom_text(aes(label = benefit, y = 2), hjust = 1,
              size = ggtext_size(textsize)) + 
    geom_text(aes(label = probORMCID1, y = 2.4), hjust = 1,
              size = ggtext_size(textsize)) +
    geom_text(aes(label = probORMCID2, y = 2.8), hjust = 1,
              size = ggtext_size(textsize)) +
    theme(axis.text.y = element_text(face = c(rep('plain', 11),"bold")),
          axis.title.x = element_text(hjust = 0),
          title = element_text(hjust = 0))
  
library(ggpubr)
ggarrange(plotlist = list(gg1, gg2), ncol = 1)  


  
## relative risk version
  
  tbl3 <- bind_rows(lapply(tbl3_rules, calculate_RRs,
                           mean_outcome_df, "death_28"))
  
  
  names(tbl3)[2] <- "lb95RR"
  names(tbl3)[3] <- "ub95RR"
  
  tbl3 %>%
    mutate(rule = tbl3_rules) %>%
    saveRDS("mimic_tbl3_rr.rds")
  
  textsize = 10
  
  ggtext_size <- function(base_size, ratio = 0.8) {
    ratio * base_size / ggplot2::.pt
  }
  
  
  tbl3 <- tbl3 %>%
    mutate(label = paste0(
      round(meanRR, 2), " (",
      round(lb95RR,2), " to ",
      round(ub95RR,2), ")")) %>%
    mutate(benefit = as.character(round(benefit, 2)),
           probMCID1 = as.character(round(probMCID1, 2)),
           probMCID2 = as.character(round(probMCID2, 2)),
           probRRMCID1 = as.character(round(probORMCID1, 2)),
           probRRMCID2 = as.character(round(probORMCID2, 2)))
  
  bind_rows(data.frame(
    meanRR = NA,
    lb95RR = NA,
    ub95RR = NA,
    benefit = "P(RR < 1.0)",
    probMCID1 = NA,
    probMCID2 = NA,
    probRRMCID1 = "P(RR < 0.9)",
    probRRMCID2 = "P(RR < 0.8)",
    label = "Relative Risk"),
    tbl3) %>%
    mutate(rule = factor(tbl3_rulenames, 
                         levels = rev(tbl3_rulenames),
                         ordered = T)) %>%
    mutate(bold = ifelse(rule == "Rule", "bold", "plain")) %>%
    ggplot(aes(y = meanRR,
               x = fct_reorder(rule, -meanRR))) +
    geom_hline(yintercept = 1, color = "gray80", size = 1) +
    geom_pointrange(aes(ymin = lb95RR,
                        ymax = ub95RR)) +
    coord_flip() +
    theme_minimal(base_size = textsize) +
    labs(y = "Posterior odds ratio (95% credible interval)",
         x = "",
         title = "Relative risk for mortality with each threshold compared to usual care: MIMIC-IV") +
    scale_y_continuous(trans = "log",
                       breaks = c(0.8, 0.9, 1.0, 1.1),
                       limits = c(0.77, 2)) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank()) +
    geom_text(aes(label = label, y = 1.11, fontface = bold), hjust = 0,
              size = ggtext_size(textsize)) +
    geom_text(aes(label = benefit, y = 1.5, fontface = bold), hjust = 1,
              size = ggtext_size(textsize)) + 
    geom_text(aes(label = probRRMCID1, y = 1.75, fontface = bold), hjust = 1,
              size = ggtext_size(textsize)) +
    geom_text(aes(label = probRRMCID2, y = 2, fontface = bold), hjust = 1,
              size = ggtext_size(textsize)) +
    theme(axis.text.y = element_text(face = c(rep('plain', 12),"bold")),
          axis.title.x = element_text(hjust = 0),
          title = element_text(hjust = 0))
  
  ggsave(filename = "plots/figure3.svg", width = 10, height = 4)
  
  
  
  imv_arr_110v88 <- calculate_ARR(rule1 = "SF.110.1",
                                  mean_outcome_df = mean_outcome_df,
                                  outcome1 = "imv_28",
                                  rule2 = "SF.88.1")
  
  death_arr_110v88 <- calculate_ARR(rule1 = "SF.110.1",
                                    mean_outcome_df = mean_outcome_df,
                                    outcome1 = "death_28",
                                    rule2 = "SF.88.1")
  
  # additional intubations per additional survivor
  -imv_arr_110v88/death_arr_110v88
  
  imv_arr_98v88 <- calculate_ARR(rule1 = "SF.98.1",
                                  mean_outcome_df = mean_outcome_df,
                                  outcome1 = "imv_28",
                                  rule2 = "SF.88.1")
  
  death_arr_98v88 <- calculate_ARR(rule1 = "SF.98.1",
                                    mean_outcome_df = mean_outcome_df,
                                    outcome1 = "death_28",
                                    rule2 = "SF.88.1")
  
  # additional intubations per additional survivor
  -imv_arr_98v88/death_arr_98v88
  
  
  # calculate e-value
  # RR + sqrt(RR * (RR - 1))
  
  RR = calculate_RRR(rule1 = "SF.88.1",
                     mean_outcome_df = mean_outcome_df,
                     outcome1 = "death_28",
                     rule2 = "SF.110.1")

  eval(RR)
  
  RR = calculate_RRR(rule1 = "SF.88.1",
                     mean_outcome_df = mean_outcome_df,
                     outcome1 = "death_28",
                     rule2 = "SF.98.1")
  
  eval(RR)
  