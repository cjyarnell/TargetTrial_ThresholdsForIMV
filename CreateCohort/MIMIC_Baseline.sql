-- this query returns all the baseline demographic info for the eligible patients
-- uses PAHRC_Timevarying
-- results saved into PAHRC_Baseline3 table

with t1 as (
  select 
  *
  from `alpine-scholar-292916.PAHRC1.PAHRC_Eligibility`
  where eligible = 1
)

SELECT
  pdx.subject_id, --ids 
  pdx.hadm_id,
  pdx.stay_id,
  pdx.eligibility_time, -- time when eligibility reached
  icu.intime,    -- icu admit time date
  icu.outtime, -- icu discharge time date
  pdx.imv,
  pdx.imv_time,
  pdx.imv_after_censor,
  bl2.death_obs,
  bl2.death_obs_time,
  bl2.icu_dc,
  bl2.icu_dc_time,
  datetime_diff(icu.outtime,icu.intime, MINUTE) as icu_discharge_time,
  icu.LOS, -- icu LOS
  adm.admission_type, -- admission type
  datetime_diff(icu.intime, adm.admittime, MINUTE) as preICU_LOS, -- admission time (can compare to icu admission time)
  mrp.curr_service, -- most responsible service
  cu.careunit,
  pt.anchor_year, -- anchor year
  pt.anchor_year_group, -- anchor year group
  pt.anchor_age, -- age
  pt.gender, -- sex
  ht.height, -- height
  we.weight_admit, -- weight
  adm.ethnicity, -- ethnicity
  adm.language, -- first language
  adm.insurance, -- insurance
  adm.marital_status, -- marital status
  adm.admission_location, -- admission location
  adm.discharge_location, -- discharge location
  case when adm.discharge_location = "DIED" then 1 else 0 end as hosp_death,
  datetime_diff(adm.dischtime, icu.intime, MINUTE) as time_to_discharge, -- discharge time
  datetime_diff(adm.deathtime, icu.intime, MINUTE) as time_to_death -- deathtime
, dx.diabetes
, dx.ckd
, dx.esrd
, dx.steroids
, dx.osa
, dx.depression
, dx.obesity
, dx.malnutrition
, dx.aml
, dx.copd
, dx.cad
, dx.chf
, dx.dnr
    , bl.spo2_bl
    , bl.fio2_bl
    , niv_bl
    , hfnc_bl
    , nrb_bl
   , resp_rate_bl
    , heart_rate_bl
    , sbp_bl
    , dbp_bl
    , mbp_bl
    , GCS_motor_bl
    , GCS_verbal_bl
    , GCS_eyes_bl
    , uo_6h_rate_bl -- uoutput
    ,  pressor_bl -- pressor
    ,  wob_bl -- wob
    ,  hb_bl -- hb
    ,  wbc_bl -- wbc
    ,  plt_bl -- plt
    ,  cr_bl --cr
    ,  lactate_bl -- lactate
    ,  pco2_bl -- pco2
    ,  ph_bl -- ph
    ,  bicarb_bl -- bicarb
    ,  trop_bl -- trop
    ,  alt_bl -- trop
    ,  glucose_bl 
    ,  albumin_bl 
    ,  temperature_bl 
    ,  cxr_bl
    ,  aline_bl
    ,  cvc_bl
    ,  foley_bl
    ,  lasix_bl
    , abx_bl
    , puffer_bl
    , opioid_bl
    , benzo_bl
    , antipsych_bl
FROM
  t1 pdx
left join ( 
  select 
      t1.stay_id
    , any_value(t1.spo2_bl) as spo2_bl
    , any_value(t1.fio2_bl) as fio2_bl
    , max(case when t1.o2_device_bl = 5 then 1 else 0 end) as niv_bl
    , max(case when t1.o2_device_bl = 4 then 1 else 0 end) as hfnc_bl
    , max(case when t1.o2_device_bl = 3 then 1 else 0 end) as nrb_bl
    , max(case when t1.o2_device_bl = 2 then 1 else 0 end) as fm_bl
    , max(case when t1.o2_device_bl = 1 then 1 else 0 end) as np_bl
    , array_agg(case when variable = "resp_rate" then value end ignore nulls order by time desc)[offset(0)] as resp_rate_bl
    , array_agg(case when variable = "heart_rate" then value end ignore nulls order by time desc)[offset(0)] as heart_rate_bl
    , array_agg(case when variable = "sbp" then value end ignore nulls order by time desc)[offset(0)] as sbp_bl
    , array_agg(case when variable = "dbp" then value end ignore nulls order by tv.time desc)[offset(0)] as dbp_bl
    , array_agg(case when variable = "mbp" then value end ignore nulls order by tv.time desc)[offset(0)] as mbp_bl
    , array_agg(case when variable = "GCS_motor" then value end ignore nulls order by tv.time desc)[offset(0)] as GCS_motor_bl
    , array_agg(case when variable = "GCS_verbal" then value end ignore nulls order by tv.time desc)[offset(0)] as GCS_verbal_bl
    , array_agg(case when variable = "GCS_eyes" then value end ignore nulls order by tv.time desc)[offset(0)] as GCS_eyes_bl
    , array_agg(case when variable = "uo_6h_rate" then value end ignore nulls order by tv.time desc)[offset(0)] as uo_6h_rate_bl -- uoutput
    , array_agg(case when variable = "pressor" then value end ignore nulls order by tv.time desc)[offset(0)] as pressor_bl -- pressor
    , array_agg(case when variable = "wob" then value end ignore nulls order by tv.time desc)[offset(0)] as wob_bl -- wob
    , array_agg(case when variable = "hb" then value end ignore nulls order by tv.time desc)[offset(0)] as hb_bl -- hb
    , array_agg(case when variable = "wbc" then value end ignore nulls order by tv.time desc)[offset(0)] as wbc_bl -- wbc
    , array_agg(case when variable = "plt" then value end ignore nulls order by tv.time desc)[offset(0)] as plt_bl -- plt
    , array_agg(case when variable = "cr" then value end ignore nulls order by tv.time desc)[offset(0)] as cr_bl --cr
    , array_agg(case when variable = "lactate" then value end ignore nulls order by tv.time desc)[offset(0)] as lactate_bl -- lactate
    , array_agg(case when variable = "pco2" then value end ignore nulls order by tv.time desc)[offset(0)] as pco2_bl -- pco2
    , array_agg(case when variable = "ph" then value end ignore nulls order by tv.time desc)[offset(0)] as ph_bl -- ph
    , array_agg(case when variable = "bicarb" then value end ignore nulls order by tv.time desc)[offset(0)] as bicarb_bl -- bicarb
    , array_agg(case when variable = "trop" then value end ignore nulls order by tv.time desc)[offset(0)] as trop_bl -- trop
    , array_agg(case when variable = "alt" then value end ignore nulls order by tv.time desc)[offset(0)] as alt_bl -- trop
    , array_agg(case when variable = "glucose" then value end ignore nulls order by tv.time desc)[offset(0)] as glucose_bl 
    , array_agg(case when variable = "albumin" then value end ignore nulls order by tv.time desc)[offset(0)] as albumin_bl 
    , array_agg(case when variable = "temperature" then value end ignore nulls order by tv.time desc)[offset(0)] as temperature_bl 
    , array_agg(case when variable = "cxr" then value end ignore nulls order by tv.time desc)[offset(0)] as cxr_bl
    , array_agg(case when variable = "aline" then value end ignore nulls order by tv.time desc)[offset(0)] as aline_bl
    , array_agg(case when variable = "cvc" then value end ignore nulls order by tv.time desc)[offset(0)] as cvc_bl
    , array_agg(case when variable = "Foley" then value end ignore nulls order by tv.time desc)[offset(0)] as foley_bl
    , array_agg(case when variable = "lasix" then value end ignore nulls order by tv.time desc)[offset(0)] as lasix_bl
    , array_agg(case when variable = "abx" then value end ignore nulls order by tv.time desc)[offset(0)] as abx_bl
    , array_agg(case when variable = "puffer" then value end ignore nulls order by tv.time desc)[offset(0)] as puffer_bl
    , array_agg(case when variable = "opioid" then value end ignore nulls order by tv.time desc)[offset(0)] as opioid_bl
    , array_agg(case when variable = "benzo" then value end ignore nulls order by tv.time desc)[offset(0)] as benzo_bl
    , array_agg(case when variable = "antipsych" then value end ignore nulls order by tv.time desc)[offset(0)] as antipsych_bl
  from t1
  left join `alpine-scholar-292916.PAHRC1.PAHRC_Timevarying` tv
  on tv.stay_id = t1.stay_id
  where time <= 0
  group by 1) bl on bl.stay_id = pdx.stay_id
left join ( 
  select 
      t1.stay_id
    , max(case when variable = "death" then 1 else 0 end) as death_obs
    , min(case when variable = "death" then time else null end) as death_obs_time
    , max(case when variable = "icu_dc" then 1 else 0 end) as icu_dc
    , min(case when variable = "icu_dc" then time else null end) as icu_dc_time    
  from t1
  left join `alpine-scholar-292916.PAHRC1.PAHRC_Timevarying` tv
  on tv.stay_id = t1.stay_id
  group by 1) bl2 on bl2.stay_id = pdx.stay_id
LEFT JOIN `physionet-data.mimic_derived.height` ht
ON pdx.stay_id = ht.stay_id
LEFT JOIN
  `physionet-data.mimic_core.admissions` adm
ON
  pdx.hadm_id = adm.hadm_id
LEFT JOIN
  `physionet-data.mimic_icu.icustays` icu
ON
  pdx.stay_id = icu.stay_id
LEFT JOIN
  `physionet-data.mimic_core.patients` pt
ON
  pdx.subject_id = pt.subject_id
left join (
SELECT 
    hadm_id
  , max(case when icd_code = "25000" 
               or icd_code = "E119" 
               or icd_code = "E1122" 
               or icd_code = "3572" 
               or icd_code = "E1165" 
               or icd_code = "25060" 
               or icd_code = "25040" 
               or icd_code = "25002" 
               or icd_code = "E1140" 
               or icd_code = "25080" 
               or icd_code = "E1151" 
               or icd_code = "25050"
        then 1 else 0 end) as diabetes
  , max(case when icd_code = "5856" 
               or icd_code = "40391" 
               or icd_code = "N186" 
               or icd_code = "V4511" 
               or icd_code = "Z992" 
               or icd_code = "I132" 
               or icd_code = "I120" 
        then 1 else 0 end) as esrd    
  , max(case when icd_code = "5856" 
               or icd_code = "40391" 
               or icd_code = "N186" 
               or icd_code = "V4511" 
               or icd_code = "Z992" 
               or icd_code = "I132" 
               or icd_code = "I120" 
               or icd_code = "40390" 
               or icd_code = "5859" 
               or icd_code = "E1122" 
               or icd_code = "I129" 
               or icd_code = "I130" 
               or icd_code = "5853" 
               or icd_code = "N183" 
               or icd_code = "5854" 
               or icd_code = "N184" 
        then 1 else 0 end) as ckd    
  , max(case when icd_code = "T380X5A" 
               or icd_code = "V5865" 
        then 1 else 0 end) as steroids    
, max(case when icd_code = "32723" 
               or icd_code = "G4733" 
        then 1 else 0 end) as osa    
  , max(case when icd_code = "311"  
        then 1 else 0 end) as depression    
  , max(case when icd_code = "E440" 
               or icd_code = "261" 
               or icd_code = "V850" 
               or icd_code = "R64" 
               or icd_code = "7994" 
               or icd_code = "Z681" 
               or icd_code = "E43" 
               or icd_code = "2639" 
        then 1 else 0 end) as malnutrition        
  , max(case when icd_code = "E669" 
               or icd_code = "27801" 
               or icd_code = "E660%" 
               or icd_code = "E662"
               or icd_code = "E661"
               or icd_code = "E668"
               or icd_code = "E669"
               or icd_code = "Z684%" 
               or icd_code = "V8535"
               or icd_code = "V8536"
               or icd_code = "V8537"
               or icd_code = "V8538"
               or icd_code = "V8539"
               or icd_code = "Z6835" 
               or icd_code = "Z6836" 
               or icd_code = "Z6837" 
               or icd_code = "Z6838" 
               or icd_code = "Z6839" 
               or icd_code = "V854%"  
        then 1 else 0 end) as obesity
  , max(case when icd_code = "41401" 
               or icd_code = "I2510" 
               or icd_code = "412" 
               or icd_code = "41400"
               or icd_code = "I252"
               or icd_code = "Z955"
               or icd_code = "I255"
               or icd_code = "4148" 
        then 1 else 0 end) as cad
  , max(case when icd_code = "4280" 
               or icd_code = "42833" 
               or icd_code = "I110" 
               or icd_code = "I5033"
               or icd_code = "I130"
               or icd_code = "42832"
               or icd_code = "4254"
               or icd_code = "I5023" 
               or icd_code = "I255"
               or icd_code = "42843"
               or icd_code = "I5032"
               or icd_code = "I509"
               or icd_code = "I429"
               or icd_code = "I5022" 
               or icd_code = "I420" 
        then 1 else 0 end) as chf
  , max(case when icd_code = "496" 
               or icd_code = "J449" 
               or icd_code = "49121" 
               or icd_code = "J441"
               or icd_code = "J440"
               or icd_code = "J439"
        then 1 else 0 end) as copd
  , max(case when icd_code = "20500" 
        then 1 else 0 end) as aml
  , max(case when icd_code = "V4986" 
               or icd_code = "Z66" 
        then 1 else 0 end) as dnr
FROM `physionet-data.mimic_hosp.diagnoses_icd`
group by 1
order by 2
) dx
on dx.hadm_id = pdx.hadm_id
LEFT JOIN 
  `physionet-data.mimic_derived.first_day_weight` we
ON 
  pdx.stay_id = we.stay_id
LEFT JOIN
  (SELECT
  hadm_id,
  curr_service
FROM 
  ( SELECT 
      pdx.hadm_id, 
      sv.curr_service,
      ROW_NUMBER() OVER (PARTITION BY pdx.hadm_id
                         ORDER BY DATETIME_DIFF(icu.intime, sv.transfertime, MINUTE)) AS rn
    FROM t1 pdx
    left join `physionet-data.mimic_icu.icustays` icu on icu.stay_id = pdx.stay_id
    LEFT JOIN `physionet-data.mimic_hosp.services` sv
    ON pdx.hadm_id = sv.hadm_id
    WHERE sv.transfertime < icu.intime
    ORDER BY pdx.hadm_id
  ) tmp
WHERE rn = 1
ORDER BY
hadm_id
  ) mrp
ON
  pdx.hadm_id = mrp.hadm_id
LEFT JOIN
  (SELECT
  hadm_id,
  careunit
FROM 
  ( SELECT 
      pdx.hadm_id, 
      tr.careunit,
    FROM t1 pdx
    left join `physionet-data.mimic_icu.icustays` icu on icu.stay_id = pdx.stay_id
    LEFT JOIN `physionet-data.mimic_core.transfers` tr
    ON pdx.hadm_id = tr.hadm_id
    WHERE icu.intime = tr.intime
    ORDER BY pdx.hadm_id
  )) cu
ON
  pdx.hadm_id = cu.hadm_id
order by 1



