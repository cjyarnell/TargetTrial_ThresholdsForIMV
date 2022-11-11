This readme describes how to fit the conditional outcome model.
The confounder model needs to be fit beforehand (because the basis function fits from the Hilbert-space Gaussian process approximation are used as time-series features).

Primary:

1) Run bart_binary_data_prep.R
2) Run mimic_BART_binary.R
3) Validate with mimic_outcome_model_validate.R

Secondary:

1) amd_bart_binary_data_prep.R
2) amd_BART_binary.R
3) amd_outcome_model_validate.R