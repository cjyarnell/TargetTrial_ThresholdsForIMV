This folder contains the files used to create the primary and secondary cohorts from the MIMIC-IV and AmsterdamUMCdb databases.

You will have to update the pathways to match your specific situation - we built the cohorts using the cloud-based Google Bigquery access to both databases.

MIMIC-IV

1) Run O2FiO2DeliveryTable
2) Run MIMIC_Eligibility
3) Run MIMIC_Timevarying
4) Run MIMIC_Baseline
5) Using those three tables (MIMIC_Eligibility, MIMIC_Timevarying, MIMIC_Baseline), run CreateCohortMIMIC

AmsterdamUMCdb

1) Preprocessing scrips: AMDS_FiO2, AMDS_goc, AMDS_ph, AMDS_po2pco2, AMDS_vitals
2) Amsterdam_Eligibility
3) Amsterdam_Timevarying
4) Amsterdam_Baseline
5) Download the Eligibility, Timevarying, and Baseline tables then run CreateCohortAMDS

Again, drives and table names may need to be adjusted to ensure smooth flow.

Christopher Yarnell

christopher.yarnell@uhn.ca