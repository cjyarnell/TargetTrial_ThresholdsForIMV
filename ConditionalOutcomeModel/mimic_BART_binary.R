# BART conditional outcome model script

setwd("~/scratch/ThresholdsTargetTrial")

library(BART)
library(tidyr)
library(dplyr)

# colours
c_light <- c("#DCBCBC")
c_light_highlight <- c("#C79999")
c_mid <- c("#B97C7C")
c_mid_highlight <- c("#A25050")
c_dark <- c("#8F2727")
c_dark_highlight <- c("#7C0000")
c_blue = "cornflower blue"

bart_df <- readRDS("data/bart_df.rds")

# filter out unneeded variables

X <- bart_df %>%
    select(-id, -partition,
           -dnr, -aml,
           -c(discharge_location:time_to_death),
           -imv_after_censor,
          -death_obs,
          -death_obs_time) %>%
    mutate(state = factor(state))

# pull out one iteration

set.seed(20220516)
rows <- sample(1:100, size = 1)

X <- X[seq(from = rows, 
                      to = nrow(X), by = 100),]

###############################################################

# Death model

X0 <- filter(X, state != 3) %>%
    select(-stay_id,
           -eventtime,
           -death28,
           -imv28,
           -imv,
           -imv_time, 
           -outtime,
           -icu_dc, 
           -icu_dc_time,
           -icu_discharge_time,
           -LOS)

outcome0 <- filter(X, state != 3)%>%
    select(stay_id, death28)

#outcome0 <- outcome0[seq(from = 1, to = nrow(outcome0), by = iter),]

M0dirichlet <- mc.pbart(x.train = data.matrix(X0),
               y.train = outcome0$death28,
               mc.cores=80,
               printevery=10,
               ntree = 200,
               sparse = TRUE,
               ndpost = 800,
                       keepevery = 50)

saveRDS(M0dirichlet, "models/bart_binM0.rds")
#}
gc()

# IMV model

X1 <- filter(X, state %in% c(0,2)) %>%
    select(-stay_id,
           -eventtime,
           -imv28,
           -imv,
           -imv_time, 
           -outtime,
           -icu_dc, 
           -icu_dc_time,
           -icu_discharge_time,
           -LOS) %>%
relocate(death28)

outcome1 <- filter(X, state %in% c(0,2))%>%
    select(stay_id, imv28)

#outcome1 <- outcome1[seq(from = 1, to = nrow(outcome1), by = iter),]

M1dirichlet <- mc.pbart(x.train = data.matrix(X1),
               y.train = outcome1$imv28,
               mc.cores=10,
               printevery=10,
               ntree = 200,
               sparse = TRUE,
               ndpost = 800,
                       keepevery = 50)

saveRDS(M1dirichlet, "models/bart_binM1_death.rds")

gc()


###########################
