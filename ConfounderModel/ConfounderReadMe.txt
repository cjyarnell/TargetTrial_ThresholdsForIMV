This folder contains the R and Stan code to fit the confounder model.

It uses the databases prepared for Stan from the files in the CreateCohort folder.

Primary:

1) Move the stan .json files to the desired working directory
2) Compile the stan program confounder.stan (or confounder_offset.stan), with STAN_THREADS=TRUE if parallelization possible
3) Run confounder_offset from the command line (perhaps using a batch script) for each subset (1,2,3). Example batch script below
4) Run process_confounder.R
5) Evaluate the confounder model with validate_confounder_model.R then confounder_validation_postprocess.R

Secondary:

1) Move the stan .json files to the desired working directory
2) Compile the stan program confounder.stan, with STAN_THREADS=TRUE if parallelization possible
3) Run compiled confounder program from the command line (perhaps using a batch script) for each subset (1,2,3). Example batch script below
4) Run amd_process_confounder.R
5) Evaluate the confounder model with amd_validate_confounder_model.R then amd_confounder_validation_postprocess.R
 