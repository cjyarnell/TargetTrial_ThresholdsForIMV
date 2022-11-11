-- generates a table with a row for every icu stay_id and columns for each of the eligibility criteria
-- (ie fiO2 40% or more, not yet invasively ventilated, receiving HFNC / NIV / NRB
-- results saved as PAHRC_Eligibility

with tv as (
select
    stay_id
  , time as charttime
  , intime
  , max(ett_flag) over (partition by stay_id order by time range unbounded preceding) as ett_flag
  , max(trach) over (partition by stay_id order by time range between unbounded preceding and 7200 following) as trach_ever
  , max(imv) over (partition by stay_id order by time range unbounded preceding) as imv
  , last_value(fio2 ignore nulls) over (partition by stay_id order by time range 480 preceding) as fio2
  , last_value(o2_device_bl ignore nulls) over (partition by stay_id order by time range 480 preceding) as o2_device_bl
  , last_value(spo2 ignore nulls) over (partition by stay_id order by time range 120 preceding) as spo2
  , last_value(GCSMotor ignore nulls) over (partition by stay_id order by time range 2880 preceding) as GCSMotor
  , max(goc) over (partition by stay_id order by time range between unbounded preceding and unbounded following ) as goc
  , last_value(phpco2 ignore nulls) over (partition by stay_id order by time range 2880 preceding) as phpco2
  from(
select 
    datetime_diff(t.charttime, i.intime, MINUTE) as time
  , i.intime
  , t.*
from (
select 
    ce.subject_id
  , ce.hadm_id
  , ce.stay_id as stay_id
  , ce.charttime
  -- o2 devices
  , NULL as ett_flag
  , null as trach
  , null as imv
  , null as o2_device_bl
  , null as fio2
  -- other vital signs
  , null as spo2
  , max(case when ce.ITEMID = 223901 and ce.valuenum <= 4 then 1 else 0 end) as GCSMotor
  , MAX(case when ce.itemid = 228687 and 
                  (ce.value = "Comfort measures only" OR ce.value = "DNAR (Do Not Attempt Resuscitation) [DNR] / DNI") 
                  then 1 
               when ce.itemid = 228687 and 
                  (ce.value = "Full code" OR ce.value = "DNAR (Do Not Attempt Resuscitation)  [DNR]") 
                  then 0 
               else null end) as goc
  , null as phpco2
    from `physionet-data.mimic_icu.chartevents` ce
left join `physionet-data.mimic_icu.icustays` pdx
on pdx.stay_id = ce.stay_id
  where ce.ITEMID in
  ( 228687, -- code status
    223901 -- GCS components
    ) and 
  ce.charttime < DATETIME_ADD(pdx.intime, INTERVAL 96 HOUR)
  group by 1,2,3,4

UNION ALL

SELECT
   pdx.subject_id as subject_id
 , pdx.hadm_id as hadm_id
 , pdx.stay_id as stay_id
 , o.charttime AS charttime

-- find the value of the time-varying exposure
    -- 0 (not intubated)
    -- 1 (intubated)
    -- 2 (tracheostomy)
  , max(case when o.o2_device = 7 then 2
             when o.o2_device = 6 then 1 
             when o.o2_device >= 1 and o.o2_device <= 5 then 0 else null end) as ett_flag
  , max(case when o.o2_device = 7 then 1 else 0 end) as trach
  , max(case when o.o2_device = 6 then 1 else 0 end) as imv
  , max(o.o2_device) as o2_device_bl
  , max(o.fio2) as fio2
  , max(o.spo2) as spo2
  , NULL as GCSMotor
  , null as goc
  , null as phpco2
FROM `physionet-data.mimic_icu.icustays` pdx
left join `alpine-scholar-292916.PAHRC1.O2DeliverySatFio2` o 
on pdx.stay_id = o.stay_id
--and  o.charttime < DATETIME_ADD(pdx.intime, INTERVAL 96 HOUR)
group by 1,2,3,4



-- add blood gas info
UNION ALL
SELECT
  pdx.subject_id as subject_id
 ,pdx.hadm_id as hadm_id
 ,pdx.stay_id as stay_id
 ,bg.charttime as charttime
 , NULL as ett_flag
  , null as trach
  , null as imv
  , null as o2_device_bl
  , null as fio2             
  , NULL as spo2
  , NULL as GCSMotor
  , null as goc
  , MAX(case when bg.pco2 >= 60 and bg.ph <= 7.20 then 1 else 0 end) as phpco2
 FROM `physionet-data.mimic_icu.icustays` pdx
LEFT JOIN `physionet-data.mimic_derived.bg` bg
ON pdx.hadm_id = bg.hadm_id
WHERE bg.charttime < DATETIME_ADD(pdx.intime, INTERVAL 96 HOUR)
group by 1,2,3,4
 ) t 
 left join `physionet-data.mimic_icu.icustays` i on i.stay_id = t.stay_id
)) 

select 
  pdx.subject_id
, pdx.hadm_id
, pdx.stay_id
, max(eligible) as eligible
, min(eligibility_time) as eligibility_time
, any_value(intime) as intime
--, array_agg(o2_device_bl order by eligible desc, pdx.charttime)[offset(0)] as o2_device_bl
--, array_agg(pdx.fio2 order by eligible desc, pdx.charttime)[offset(0)] as fio2_bl
--, array_agg(pdx.spo2 order by eligible desc, pdx.charttime)[offset(0)] as spo2_bl
, min(eligibility_admission) as eligibility_admission
, min(eligibility_age) as eligibility_age
, min(eligibility_outcome) as eligibility_outcome
, max(eligibility_trach) as eligibility_trach
, max(goc_eligible) as goc_eligibility
, max(case when o.o2_device = 6 and datetime_diff(o.charttime, pdx.intime, minute) - eligibility_time <= 96*60 then 1 else 0 end) as imv
, min(case when o.o2_device = 6 then datetime_diff(o.charttime, pdx.intime, minute) - eligibility_time else null end) as imv_time
, max(case when o.o2_device = 6
            and datetime_diff(o.charttime, pdx.intime, minute) - eligibility_time > 96*60 
            and datetime_diff(o.charttime, pdx.intime, minute) - eligibility_time < 24*28*60 
            then 1 else 0 end) as imv_after_censor
from (
select
  subject_id
, hadm_id
, stay_id
, charttime
, intime
, case when 
      (row_number() over 
      (partition by subject_id order by eligible_moment desc, hospstay_seq, icustay_seq) 
      = 1) and eligible_moment = 1 then 1 else 0 end as eligible
, case when 
      (row_number() over 
      (partition by subject_id order by eligible_moment desc, hospstay_seq, icustay_seq) 
      = 1) and eligible_moment = 1 then charttime else null end as eligibility_time
, o2_device_bl
, spo2
, fio2
, eligibility_admission
, eligibility_age
, eligibility_outcome
, eligibility_trach
, ineligible_imv
, loc_eligible
, phpco2_eligible
, goc_eligible
from (select
  subject_id
, hadm_id
, stay_id
, intime
, charttime
, case when eligibility_admission = 1 
        and eligibility_age = 1
        and eligibility_outcome = 1  
        and eligibility_o2_device = 1      
        and sfratio_eligible = 1
        and ineligible_imv = 0
        and eligibility_trach = 1
        and loc_eligible = 1
        and phpco2_eligible = 1
        and goc_eligible = 1
        and charttime < 1440
        then 1 else 0 end as eligible_moment
, hospstay_seq
, icustay_seq
, o2_device_bl
, fio2
, spo2
, imv
, eligibility_admission
, eligibility_age
, eligibility_outcome
, eligibility_trach
, ineligible_imv
, loc_eligible
, phpco2_eligible
, goc_eligible
from(
SELECT 
   ad.subject_id
 , ad.hadm_id
 , tv.intime
 , istay.hospstay_seq
 , case when ad.admission_type in ("SURGICAL SAME DAY ADMISSION","ELECTIVE") then 0 else 1
      end as eligibility_admission
 , case when ad.discharge_location is not null then 1 else 0 end as eligibility_outcome
 , tv.stay_id
 , istay.icustay_seq
 , case when istay.admission_age >= 18 then 1 else 0 end as eligibility_age
 , tv.charttime
 , case when o2_device_bl >= 3 and o2_device_bl < 6 then 1 else 0 end as eligibility_o2_device
 , case when fio2 >= 40 then 1 else 0 end as sfratio_eligible
 , max(imv) over (partition by tv.stay_id order by tv.charttime range unbounded preceding) as ineligible_imv
 , case when trach_ever = 1 then 0 else 1 end as eligibility_trach
 , case when GCSMotor = 1 then 0 else 1 end as loc_eligible
 , case when phpco2 = 1 then 0 else 1 end as phpco2_eligible
 , case when goc = 1 then 0 else 1 end as goc_eligible
 , goc
 , ett_flag
 , o2_device_bl
 , fio2
 , spo2
 , imv
FROM tv
left join physionet-data.mimic_derived.icustay_detail istay on istay.stay_id = tv.stay_id
left join physionet-data.mimic_core.admissions ad on ad.hadm_id = istay.hadm_id
))
) pdx 
left join `alpine-scholar-292916.PAHRC1.O2DeliverySatFio2` o
on o.stay_id = pdx.stay_id
--where eligible = 1
group by 3,2,1
order by 4 desc, 1