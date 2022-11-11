This folder contains the files needed to perform the nonparametric G computation for both invasive ventilation and mortality.

You need to have the confounder model and conditional model already fit in order for this to work.

The G-computation is an integral that is numerically solved with Monte Carlo integration.
Note that hsgp_fixedGP.stan is missing as of 2022-11-11 due to an accidental deletion on the computer cluster.

Primary:

1) run simulate_trajectory.R to generate simulated patient trajectories from the confounder model
2) run predict_outcomes_binary to apply thresholds and generate mortality and invasive ventilation predictions for every trajectory

Secondary:

1) run amd_simulate_trajectory.R to generate simulated patient trajectories from the confounder model
2) run amd_predict_outcomes_binary to apply thresholds and generate mortality and invasive ventilation predictions for every trajectory

