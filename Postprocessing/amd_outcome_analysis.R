# IMV outcomes analysis file

setwd("C:/Git/PAHRC/imvThresholdsTargetTrial")
library(tidyverse)
library(kableExtra)

# colours
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")
c_blue = "cornflower blue"

source("outcome_analysis_functions.R")

pth <- "C:/Users/chris/OneDrive/Documents/LargeDataFiles/amd_predictedOutcomes"
pth2 <- "C:/Users/chris/OneDrive/Documents/LargeDataFiles/amd_predictedOutcomes1-200"
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

saveRDS(mean_outcome_df, "amd_mean_outcome_df.rds")

mean_outcome_df <- readRDS("amd_mean_outcome_df.rds")

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

library(ggridges)

plot_all_amd(mean_outcome_df)
ggsave("plots/amd_prelim_outcomes.svg", width = 10, height = 7)

sf_outcomes <- filter(outcome_df, rule %in% c("SF.88.1",
                                              "SF.98.1",
                                              "SF.110.1"))
# simple Figure 1: only SF outcomes

make_figure_1(sf_outcomes) +
  labs(title = "Outcomes at 28 days: AmsterdamUMCdb")
ggsave("plots/amd_figure1.svg", width = 8, height = 8)

   # Table 1 : primary outcomes and secondary outcomes
make_table_1(mean_outcome_df) %>%
  write.csv("amd_table2.csv")


rules = unique(mean_outcome_df$rule)

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
                         mean_outcome_df, "death_28"))


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

bind_rows(data.frame(
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
       title = "Odds ratios for mortality with each threshold compared to usual care: AmsterdamUMCdb") +
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


ggsave(filename = "plots/amd_figure3.svg", width = 10, height = 4)


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

