-- This query extracts the timevarying data for 48h prior to and 96h following eligibility --
-- Results are saved in PAHRC_Timevarying

with pdx as (
  select *
  from `alpine-scholar-292916.PAHRC1.PAHRC_Eligibility`
  where eligible = 1
)

select 
  pdx.stay_id
, time
, variable
, max(value) as value -- the work of breathing variables have multiple ways to be counted, so aggregate to abnormal
from(
select
    pdx.stay_id
  , datetime_diff(ce.charttime, intime, minute) - eligibility_time as time
  , case when ce.itemid in (220210,224690) and valuenum > 0 and valuenum < 70 then "resp_rate"
         when ce.itemid in (220277) and valuenum > 0 and valuenum <= 100 then "spo2" 
         when ce.itemid in (220045) and valuenum > 0 and valuenum < 300 then "heart_rate"
         when ce.itemid in (220179,220050) and valuenum > 0 and valuenum < 400 then "sbp" 
         when ce.itemid in (220180,220051) and valuenum > 0 and valuenum < 400 then "dbp" 
         when ce.itemid in (220052,220181,225312) and valuenum > 0 and valuenum < 400 then "mbp" 
         when ce.itemid in (223900) then "GCS_verbal"
         when ce.itemid in (223901) then "GCS_motor"
         when ce.itemid in (220739) then "GCS_eyes"
         when ce.itemid in (229322, 223990, 229323) then "wob"
         when ce.itemid in (223761, 223762) then "temperature"
end as variable

  , case when ce.itemid in (220210,224690) and valuenum > 0 and valuenum < 70 then valuenum -- rr
         when ce.itemid in (220277) and valuenum > 0 and valuenum <= 100 then valuenum  -- spo2
         when ce.itemid in (220045) and valuenum > 0 and valuenum < 300 then valuenum  -- hr
         when ce.itemid in (220179,220050) and valuenum > 0 and valuenum < 400 then valuenum --sbp
         when ce.itemid in (220180,220051) and valuenum > 0 and valuenum < 400 then valuenum --dbp
         when ce.itemid in (220052,220181,225312) and valuenum > 0 and valuenum < 400 then valuenum --mbp
         when ce.itemid in (223900) then valuenum
         when ce.itemid in (223901) then valuenum
         when ce.itemid in (220739) then valuenum
         when ce.itemid in (229322) and ce.value IN (
           'Agonal', 'Discoordinate', 'Gasping efforts',
           'Prolonged exhalation', 'Shallow', 'Irregular',
           'Nasal flaring', 'Cheyne-Stokes', 
           'Accessory muscle use/retractions', 'Frequent desaturation episodes',
           'Inability to speak in full sentences', 'Active exhalation') then 1
         when ce.itemid in (229322) and ce.value IN ('Regular') then 0
         when ce.itemid in (223990) and ce.value IN ('Dyspneic','Agonal', 'Labored', 'Shallow', 'Apneic') then 1
         when ce.itemid in (223990) and ce.value IN ('Normal') then 0
         when ce.itemid in (229323) and ce.value IN ('Moderate - 4', 'Moderate - 5', 'Moderate - 6', 'Moderate - 7', 'Severe - 8', 'Severe - 9', 'Severe - 10') then 1
         when ce.itemid in (229323) and ce.value IN ('None - 0', 'Mild - 1', 'Mild - 2', 'Mild - 3') then 0
         when ce.itemid in (223761) and valuenum > 70 and valuenum < 120 then (valuenum-32)/1.8
         when ce.itemid in (223762) and valuenum > 10 and valuenum < 50  then valuenum
         end as value

from pdx
left join (
  select * from `physionet-data.mimic_icu.chartevents` ce
  where ce.itemid IN (
    220210,224690 -- Respiratory rate
  , 220045 -- heart rate
  , 220277 -- spo2
  , 220179,220050 -- sbp
  , 220180,220051 -- dbp
  , 220052,220181,225312 -- mbp
  , 223901 -- GCS motor
  , 223900 -- GCS verbal
  , 220739 -- GCS eye
  , 229322, 223990, 229323 -- work of breathing
  , 223761, 223762 -- temperature
 -- , rhythm
 -- , sputum
 -- , cough
  )) ce
on pdx.stay_id = ce.stay_id
where datetime_diff(ce.charttime, intime, minute) > -48*60 
   and datetime_diff(ce.charttime, intime, minute) - eligibility_time <= 96*60

UNION ALL

select 
    pdx.stay_id
  , datetime_diff(deathtime, pdx.intime, minute) - eligibility_time as time
  , "death" as variable
  , 1 as value
from pdx
left join `physionet-data.mimic_core.admissions` adm
on pdx.hadm_id = adm.hadm_id
where datetime_diff(deathtime, pdx.intime, minute) - eligibility_time <= 96*60

UNION ALL

select 
    pdx.stay_id
  , datetime_diff(outtime, pdx.intime, minute) - eligibility_time as time
  , "icu_dc" as variable
  , 1 as value
from pdx
left join `physionet-data.mimic_icu.icustays` icu
on pdx.stay_id = icu.stay_id
where datetime_diff(outtime, pdx.intime, minute) - eligibility_time <= 96*60

UNION ALL

select 
    pdx.stay_id
  , datetime_diff(ce.charttime, intime, minute) - eligibility_time as time
  , case when itemid in (50818) then "pco2" 
         when itemid in (50820) then "ph"
         when itemid in (50813) then "lactate"
         when itemid in (51222) then "hb"
         when itemid in (51301) then "wbc"
         when itemid in (51265) then "plt"
         when itemid in (50912) then "cr"
         when itemid in (50882) then "bicarb"
         when itemid in (51003) then "trop"
         when itemid in (50861) then "alt"
         when itemid in (50885) then "bili"
         when itemid in (50862) then "albumin"
         when itemid in (50931) then "glucose"
         end as variable
  , case when itemid in (    50818  -- pco2
  , 50820  -- ph
  , 50813  -- lactate
  , 51222 -- hb
  , 51301 -- wbc
  , 51265 -- plt
  , 50912 -- cr
  , 50882 -- bicarb
  , 51003 -- trop
  , 50861 -- alt
  , 50885 -- bili
  , 50862 -- alb
  , 50931 -- glucose
  ) and valuenum > 0 then valuenum end value
from pdx
left join (
  select * from `physionet-data.mimic_hosp.labevents` ce
  where itemid IN (
    50818  -- pco2
  , 50820  -- ph
  , 50813  -- lactate
  , 51222 -- hb
  , 51301 -- wbc
  , 51265 -- plt
  , 50912 -- cr
  , 50882 -- bicarb
  , 51003 -- trop
  , 50861 -- alt
  , 50885 -- bili
  , 50862 -- alb
  , 50931 -- glucose
  )) ce
on pdx.subject_id = ce.subject_id
where datetime_diff(ce.charttime, intime, minute) > -48*60 
   and datetime_diff(ce.charttime, intime, minute) - eligibility_time <= 96*60


UNION ALL 

select 
    pdx.stay_id
  , datetime_diff(o.charttime, intime, minute) - eligibility_time as time
  , case when o.fio2 is not null then "fio2" end as variable
  , o.fio2 as value
from pdx 
left join (select * from `alpine-scholar-292916.PAHRC1.O2DeliverySatFio2`
          where fio2 is not null) o
on pdx.stay_id = o.stay_id
where datetime_diff(o.charttime, intime, minute) > -48*60 
   and datetime_diff(o.charttime, intime, minute) - eligibility_time <= 96*60


UNION ALL 

select 
    pdx.stay_id
  , datetime_diff(o.charttime, intime, minute) - eligibility_time as time
  , case when o.o2_device is not null then "o2_device" end as variable
  , o.o2_device as value
from pdx 
left join (select * from `alpine-scholar-292916.PAHRC1.O2DeliverySatFio2`
          where o2_device is not null) o 
on pdx.stay_id = o.stay_id
where datetime_diff(o.charttime, intime, minute) > -48*60 
   and datetime_diff(o.charttime, intime, minute) - eligibility_time <= 96*60


UNION ALL 

select 
    pdx.stay_id
  , datetime_diff(ce.charttime, intime, minute) - eligibility_time as time
  , case when uo_mlkghr_6hr > 0 and uo_mlkghr_6hr < 50 then "uo_6h_rate" end as variable
  , case when uo_mlkghr_6hr > 0 and uo_mlkghr_6hr < 50 then uo_mlkghr_6hr end as value
from pdx
left join `physionet-data.mimic_derived.urine_output_rate` ce
  on pdx.stay_id = ce.stay_id
where datetime_diff(ce.charttime, intime, minute) > -48*60 
   and datetime_diff(ce.charttime, intime, minute) - eligibility_time <= 96*60


UNION ALL 

select 
    pdx.stay_id
  , datetime_diff(pe.starttime, intime, minute) - eligibility_time as time
  , case when itemid in (225459, 229581) then "cxr" 
         when itemid in (225752) then "aline"
         when itemid in (224263) then "cvc"
         when itemid in (229351) then "Foley" end as variable
  , case when itemid in (225459, 229581) then 1 
         when itemid in (225752) then 1
         when itemid in (224263) then 1
         when itemid in (229351) then 1 end as value
from pdx
left join (select * from `physionet-data.mimic_icu.procedureevents`
          where itemid in (
    225459 -- cxr
  , 229581 -- portable cxr
  , 225752 -- arterial line
  , 224263 -- multilumen central line
  , 229351 -- Foley
  )) pe
  on pdx.stay_id = pe.stay_id
where datetime_diff(pe.starttime, intime, minute) > -48*60 
   and datetime_diff(pe.starttime, intime, minute) - eligibility_time <= 96*60


UNION ALL 

select 
    pdx.stay_id
  , datetime_diff(pr.charttime, intime, minute) - eligibility_time as time
  , "pressor" as variable
  , case when ne_on_off = 1 or vp_on_off = 1 or dp_on_off = 1 or ph_on_off = 1 or ep_on_off = 1 then 1
    else 0 end as value
from pdx
LEFT JOIN (select 
  stay_id
, charttime 
-- use the cases to account for the times when the start / stop times are entered in error, eg stop time after start time for next dose
-- these are rare (13/~2500 pressor dose changes in this cohort)
, case when sum(ne_on_off) over (partition by stay_id order by charttime range unbounded preceding) >= 1 then 1 
       when sum(ne_on_off) over (partition by stay_id order by charttime range unbounded preceding) <= 0 then 0
       end as ne_on_off
, case when sum(vp_on_off) over (partition by stay_id order by charttime range unbounded preceding) >= 1 then 1 
       when sum(vp_on_off) over (partition by stay_id order by charttime range unbounded preceding) <= 0 then 0
       end as vp_on_off
, case when sum(dp_on_off) over (partition by stay_id order by charttime range unbounded preceding) >= 1 then 1 
       when sum(dp_on_off) over (partition by stay_id order by charttime range unbounded preceding) <= 0 then 0
       end as dp_on_off
, case when sum(ph_on_off) over (partition by stay_id order by charttime range unbounded preceding) >= 1 then 1 
       when sum(ph_on_off) over (partition by stay_id order by charttime range unbounded preceding) <= 0 then 0
       end as ph_on_off
, case when sum(ep_on_off) over (partition by stay_id order by charttime range unbounded preceding) >= 1 then 1 
       when sum(ep_on_off) over (partition by stay_id order by charttime range unbounded preceding) <= 0 then 0
       end as ep_on_off
from
(select
    stay_id
  , charttime 
  , max(ne_on) - max(ne_off) as ne_on_off
  , max(vp_on) - max(vp_off) as vp_on_off
  , max(dp_on) - max(dp_off) as dp_on_off
  , max(ph_on) - max(ph_off) as ph_on_off
  , max(ep_on) - max(ep_off) as ep_on_off
from(select 
     pdx.stay_id
   , ne1.starttime as charttime
   , case when ne1.starttime is not null then 1 else 0 end as ne_on
   , 0 as ne_off
   , null as vp_on
   , null as vp_off
   , null as dp_on
   , null as dp_off
   , null as ph_on
   , null as ph_off
   , null as ep_on
   , null as ep_off
  from pdx
   left join `physionet-data.mimic_derived.norepinephrine` ne1
   on pdx.stay_id = ne1.stay_id
   where datetime_diff(ne1.starttime, intime, minute) > -48*60 
   and datetime_diff(ne1.starttime, intime, minute) - eligibility_time <= 96*60


  union all 

select 
     pdx.stay_id
   , ne1.endtime as charttime
   , 0 as ne_on
   , case when ne1.endtime is not null then 1 else 0 end as ne_off
   , null as vp_on
   , null as vp_off
   , null as dp_on
   , null as dp_off
   , null as ph_on
   , null as ph_off
   , null as ep_on
   , null as ep_off
   from pdx
   left join `physionet-data.mimic_derived.norepinephrine` ne1
   on pdx.stay_id = ne1.stay_id
   where datetime_diff(ne1.endtime, intime, minute) > -48*60 
   and datetime_diff(ne1.endtime, intime, minute) - eligibility_time <= 96*60


union all 

select 
     pdx.stay_id
   , starttime as charttime
   , null as ne_on
   , null as ne_off
   , case when starttime is not null then 1 else 0 end as vp_on
   , 0 as vp_off
   , null as dp_on
   , null as dp_off
   , null as ph_on
   , null as ph_off
   , null as ep_on
   , null as ep_off
  from pdx
   left join      `physionet-data.mimic_derived.vasopressin`  ne1
   on pdx.stay_id = ne1.stay_id
   where datetime_diff(ne1.starttime, intime, minute) > -48*60 
   and datetime_diff(ne1.starttime, intime, minute) - eligibility_time <= 96*60


  union all 

select 
     pdx.stay_id
   , endtime as charttime
   , null as ne_on
   , null as ne_off
   , 0 as vp_on
   , case when endtime is not null then 1 else 0 end as vp_off
   , null as dp_on
   , null as dp_off
   , null as ph_on
   , null as ph_off
   , null as ep_on
   , null as ep_off
    from pdx
   left join `physionet-data.mimic_derived.vasopressin`  ne1
   on pdx.stay_id = ne1.stay_id
   where datetime_diff(ne1.endtime, intime, minute) > -48*60 
   and datetime_diff(ne1.endtime, intime, minute) - eligibility_time <= 96*60


     

union all 

select 
     pdx.stay_id
   , starttime as charttime
   , null as ne_on
   , null as ne_off
   , null as vp_on
   , null as vp_off
   , case when starttime is not null then 1 else 0 end as dp_on
   , 0 as dp_off
   , null as ph_on
   , null as ph_off
   , null as ep_on
   , null as ep_off
     from pdx
   left join      `physionet-data.mimic_derived.dopamine` ne1
   on pdx.stay_id = ne1.stay_id
   where datetime_diff(ne1.starttime, intime, minute) > -48*60 
   and datetime_diff(ne1.starttime, intime, minute) - eligibility_time <= 96*60




  union all 

select 
     pdx.stay_id
   , endtime as charttime
   , null as ne_on
   , null as ne_off
   , null as vp_on
   , null as vp_off
   , 0 as dp_on
   , case when endtime is not null then 1 else 0 end as dp_off
   , null as ph_on
   , null as ph_off
   , null as ep_on
   , null as ep_off
    from pdx
   left join      `physionet-data.mimic_derived.dopamine`   ne1
   on pdx.stay_id = ne1.stay_id
   where datetime_diff(ne1.endtime, intime, minute) > -48*60 
   and datetime_diff(ne1.endtime, intime, minute) - eligibility_time <= 96*60
 


union all 

select 
     pdx.stay_id
   , starttime as charttime
   , null as ne_on
   , null ne_off
   , null as vp_on
   , null as vp_off
   , null as dp_on
   , null as dp_off
   , case when starttime is not null then 1 else 0 end as ph_on
   , 0 as ph_off
   , null as ep_on
   , null as ep_off
  from pdx
   left join           `physionet-data.mimic_derived.phenylephrine` ne1
   on pdx.stay_id = ne1.stay_id
   where datetime_diff(ne1.starttime, intime, minute) > -48*60 
   and datetime_diff(ne1.starttime, intime, minute) - eligibility_time <= 96*60


  union all 

select 
     pdx.stay_id
   , endtime as charttime
   , null as ne_on
   , null ne_off
   , null as vp_on
   , null as vp_off
   , null as dp_on
   , null as dp_off
   , 0 as ph_on
   , case when endtime is not null then 1 else 0 end as ph_off
   , null as ep_on
   , null as ep_off
   from pdx
   left join           `physionet-data.mimic_derived.phenylephrine`   ne1
   on pdx.stay_id = ne1.stay_id
   where datetime_diff(ne1.endtime, intime, minute) > -48*60 
   and datetime_diff(ne1.endtime, intime, minute) - eligibility_time <= 96*60
 

union all 

select 
     pdx.stay_id
   , starttime as charttime
   , null as ne_on
   , null ne_off
   , null as vp_on
   , null as vp_off
   , null as dp_on
   , null as dp_off
   , null as ph_on
   , null as ph_off
   , case when starttime is not null then 1 else 0 end as ep_on
   , 0 as ep_off
   from pdx
   left join        `physionet-data.mimic_derived.epinephrine` ne1
   on pdx.stay_id = ne1.stay_id
   where datetime_diff(ne1.starttime, intime, minute) > -48*60 
   and datetime_diff(ne1.starttime, intime, minute) - eligibility_time <= 96*60



  union all 

select 
     pdx.stay_id
   , endtime as charttime
   , null as ne_on
   , null ne_off
   , null as vp_on
   , null as vp_off
   , null as dp_on
   , null as dp_off
   , null as ph_on
   , null as ph_off
   , 0 as ep_on
   , case when endtime is not null then 1 else 0 end as ep_off
      from pdx
   left join               `physionet-data.mimic_derived.epinephrine`    ne1
   on pdx.stay_id = ne1.stay_id
   where datetime_diff(ne1.endtime, intime, minute) > -48*60 
   and datetime_diff(ne1.endtime, intime, minute) - eligibility_time<= 96*60

)
group by 1,2
)) pr
ON pdx.stay_id = pr.stay_id
where datetime_diff(pr.charttime, intime, minute) > -48*60 
   and datetime_diff(pr.charttime, intime, minute) - eligibility_time <= 96*60


UNION ALL 

select 
    pdx.stay_id
  , datetime_diff(emar.charttime, intime, minute) - eligibility_time as time
  , case when lower(emar.medication) like "%furosemide%" or
                  lower(emar.medication) like "%torsemide%"  or
                  lower(emar.medication) like "%bumetanide%" then "lasix"
         when lower(emar.medication) like "%albuterol%" then "puffer"
         when lower(emar.medication) like "%lorazepam%" or 
                  lower(emar.medication) like "%ativan%" or 
                  lower(emar.medication) like "%midazolam%" or 
                  lower(emar.medication) like "%clonazepam%" or
                  lower(emar.medication) like "%diazepam%" or 
                  lower(emar.medication) like "%lorazepam%" then "benzo"
         when lower(emar.medication) like "%fentanyl%" or 
                  lower(emar.medication) like "%hydromorphone%" or 
                  lower(emar.medication) like "%hydrocodone%" or
                  lower(emar.medication) like "%oxycodone%" or 
                  lower(emar.medication) like "%morphine%" then "opioid" 
        when lower(emar.medication) like "%aripiprazole%" or 
                  lower(emar.medication) like "%clozapine%" or 
                  lower(emar.medication) like "%droperidol%" or
                  lower(emar.medication) like "%haloperidol%" or 
                  lower(emar.medication) like "%loxapine%" or 
                  lower(emar.medication) like "%olanzapine%" or 
                  lower(emar.medication) like "%quetiapine%" or 
                  lower(emar.medication) like "%risperidone%"
                  then "antipsych"
        when lower(emar.medication) like '%adoxa%' 
      or lower(emar.medication) like '%ala-tet%' 
      or  lower(emar.medication) like '%alodox%' 
      or  lower(emar.medication) like '%amikacin%' 
      or  lower(emar.medication) like '%amikin%' 
      or  lower(emar.medication) like '%amoxicill%' 
      or  lower(emar.medication) like '%ancef%' 
      or  lower(emar.medication) like '%clavulanate%' 
      or  lower(emar.medication) like '%ampicillin%' 
      or  lower(emar.medication) like '%augmentin%' 
      or  lower(emar.medication) like '%avelox%' 
      or  lower(emar.medication) like '%avidoxy%' 
      or  lower(emar.medication) like '%azactam%' 
      or  lower(emar.medication) like '%azithromycin%' 
      or  lower(emar.medication) like '%aztreonam%' 
      or  lower(emar.medication) like '%axetil%' 
      or  lower(emar.medication) like '%bactocill%' 
      or  lower(emar.medication) like '%bactrim%' 
      or  lower(emar.medication) like '%bactroban%' 
      or  lower(emar.medication) like '%bethkis%' 
      or  lower(emar.medication) like '%biaxin%' 
      or  lower(emar.medication) like '%bicillin l-a%' 
      or  lower(emar.medication) like '%cayston%' 
      or  lower(emar.medication) like '%cefazolin%' 
      or  lower(emar.medication) like '%cedax%' 
      or  lower(emar.medication) like '%cefoxitin%' 
      or  lower(emar.medication) like '%ceftazidime%' 
      or  lower(emar.medication) like '%cefaclor%' 
      or  lower(emar.medication) like '%cefadroxil%' 
      or  lower(emar.medication) like '%cefdinir%' 
      or  lower(emar.medication) like '%cefditoren%' 
      or  lower(emar.medication) like '%cefepime%' 
      or  lower(emar.medication) like '%cefotan%' 
      or  lower(emar.medication) like '%cefotetan%' 
      or lower(emar.medication) like '%cefotaxime%' 
      or lower(emar.medication) like '%ceftaroline%' 
      or lower(emar.medication) like '%cefpodoxime%' 
      or lower(emar.medication) like '%cefpirome%' 
      or lower(emar.medication) like '%cefprozil%' 
      or lower(emar.medication) like '%ceftibuten%' 
      or lower(emar.medication) like '%ceftin%' 
      or lower(emar.medication) like '%ceftriaxone%' 
      or lower(emar.medication) like '%cefuroxime%' 
      or lower(emar.medication) like '%cephalexin%' 
      or lower(emar.medication) like '%cephalothin%' 
      or lower(emar.medication) like '%cephapririn%' 
      or lower(emar.medication) like '%chloramphenicol%' 
      or lower(emar.medication) like '%cipro%' 
      or lower(emar.medication) like '%ciprofloxacin%' 
      or lower(emar.medication) like '%claforan%' 
      or lower(emar.medication) like '%clarithromycin%' 
      or lower(emar.medication) like '%cleocin%' 
      or lower(emar.medication) like '%clindamycin%' 
      or lower(emar.medication) like '%cubicin%' 
      or lower(emar.medication) like '%dicloxacillin%' 
      or lower(emar.medication) like '%dirithromycin%' 
      or lower(emar.medication) like '%doryx%' 
      or lower(emar.medication) like '%doxycy%' 
      or lower(emar.medication) like '%duricef%' 
      or lower(emar.medication) like '%dynacin%' 
      or lower(emar.medication) like '%ery-tab%' 
      or lower(emar.medication) like '%eryped%' 
      or lower(emar.medication) like '%eryc%' 
      or lower(emar.medication) like '%erythrocin%' 
      or lower(emar.medication) like '%erythromycin%' 
      or lower(emar.medication) like '%factive%' 
      or lower(emar.medication) like '%flagyl%' 
      or lower(emar.medication) like '%fortaz%' 
      or lower(emar.medication) like '%furadantin%' 
      or lower(emar.medication) like '%garamycin%' 
      or lower(emar.medication) like '%gentamicin%' 
      or lower(emar.medication) like '%kanamycin%' 
      or lower(emar.medication) like '%keflex%' 
      or lower(emar.medication) like '%kefzol%' 
      or lower(emar.medication) like '%ketek%' 
      or lower(emar.medication) like '%levaquin%' 
      or lower(emar.medication) like '%levofloxacin%' 
      or lower(emar.medication) like '%lincocin%' 
      or lower(emar.medication) like '%linezolid%' 
      or lower(emar.medication) like '%macrobid%' 
      or lower(emar.medication) like '%macrodantin%' 
      or lower(emar.medication) like '%maxipime%' 
      or lower(emar.medication) like '%mefoxin%' 
      or lower(emar.medication) like '%metronidazole%' 
      or lower(emar.medication) like '%meropenem%' 
      or lower(emar.medication) like '%methicillin%' 
      or lower(emar.medication) like '%minocin%' 
      or lower(emar.medication) like '%minocycline%' 
      or lower(emar.medication) like '%monodox%' 
      or lower(emar.medication) like '%monurol%' 
      or lower(emar.medication) like '%morgidox%' 
      or lower(emar.medication) like '%moxatag%' 
      or lower(emar.medication) like '%moxifloxacin%' 
      or lower(emar.medication) like '%mupirocin%' 
      or lower(emar.medication) like '%myrac%' 
      or lower(emar.medication) like '%nafcillin%' 
      or lower(emar.medication) like '%neomycin%' 
      or lower(emar.medication) like '%nicazel doxy 30%' 
      or lower(emar.medication) like '%nitrofurantoin%' 
      or lower(emar.medication) like '%norfloxacin%' 
      or lower(emar.medication) like '%noroxin%' 
      or lower(emar.medication) like '%ocudox%' 
      or lower(emar.medication) like '%ofloxacin%' 
      or lower(emar.medication) like '%omnicef%' 
      or lower(emar.medication) like '%oracea%' 
      or lower(emar.medication) like '%oraxyl%' 
      or lower(emar.medication) like '%oxacillin%' 
      or lower(emar.medication) like '%pc pen vk%' 
      or lower(emar.medication) like '%pce dispertab%' 
      or lower(emar.medication) like '%panixine%' 
      or lower(emar.medication) like '%pediazole%' 
      or lower(emar.medication) like '%penicillin%' 
      or lower(emar.medication) like '%periostat%' 
      or lower(emar.medication) like '%pfizerpen%' 
      or lower(emar.medication) like '%piperacillin%' 
      or lower(emar.medication) like '%tazobactam%' 
      or lower(emar.medication) like '%primsol%' 
      or lower(emar.medication) like '%proquin%' 
      or lower(emar.medication) like '%raniclor%' 
      or lower(emar.medication) like '%rifadin%' 
      or lower(emar.medication) like '%rifampin%' 
      or lower(emar.medication) like '%rocephin%' 
      or lower(emar.medication) like '%smz-tmp%' 
      or lower(emar.medication) like '%septra%' 
      or lower(emar.medication) like '%septra ds%' 
      or lower(emar.medication) like '%septra%' 
      or lower(emar.medication) like '%solodyn%' 
      or lower(emar.medication) like '%spectracef%' 
      or lower(emar.medication) like '%streptomycin%' 
      or lower(emar.medication) like '%sulfadiazine%' 
      or lower(emar.medication) like '%sulfamethoxazole%' 
      or lower(emar.medication) like '%trimethoprim%' 
      or lower(emar.medication) like '%sulfatrim%' 
      or lower(emar.medication) like '%sulfisoxazole%' 
      or lower(emar.medication) like '%suprax%' 
      or lower(emar.medication) like '%synercid%' 
      or lower(emar.medication) like '%tazicef%' 
      or lower(emar.medication) like '%tetracycline%' 
      or lower(emar.medication) like '%timentin%' 
      or lower(emar.medication) like '%tobramycin%' 
      or lower(emar.medication) like '%trimethoprim%' 
      or lower(emar.medication) like '%unasyn%' 
      or lower(emar.medication) like '%vancocin%' 
      or lower(emar.medication) like '%vancomycin%' 
      or lower(emar.medication) like '%vantin%' 
      or lower(emar.medication) like '%vibativ%' 
      or lower(emar.medication) like '%vibra-tabs%' 
      or lower(emar.medication) like '%vibramycin%' 
      or lower(emar.medication) like '%zinacef%' 
      or lower(emar.medication) like '%zithromax%' 
      or lower(emar.medication) like '%zosyn%' 
      or lower(emar.medication) like '%zyvox%' then "abx"
                  end as variable
  , 1 as value
from pdx
left join (select * from `physionet-data.mimic_hosp.emar` emar
         where (lower(emar.medication) like "%furosemide%"
    or lower(emar.medication) like "%albuterol%"
    or lower(emar.medication) like "%lorazepam%"
    or lower(emar.medication) like  "%ativan%"
    or lower(emar.medication) like "%clonazepam%"
    or lower(emar.medication) like  "%diazepam%" 
    or lower(emar.medication) like "%lorazepam%"
    or lower(emar.medication) like  "%fentanyl%"
    or lower(emar.medication) like  "%hydromorphone%" 
    or lower(emar.medication) like "%hydrocodone%"
    or lower(emar.medication) like "%oxycodone%"
    or lower(emar.medication) like  "%morphine%"
    or lower(emar.medication) like  "%aripiprazole%"
    or lower(emar.medication) like  "%clozapine%"
    or lower(emar.medication) like  "%droperidol%"
    or lower(emar.medication) like  "%haloperidol%"
    or lower(emar.medication) like  "%loxapine%"
    or lower(emar.medication) like  "%olanzapine%"
    or lower(emar.medication) like  "%quetiapine%"
    or lower(emar.medication) like "%risperidone%"
       or lower(emar.medication) like '%adoxa%' 
      or lower(emar.medication) like '%ala-tet%' 
      or  lower(emar.medication) like '%alodox%' 
      or  lower(emar.medication) like '%amikacin%' 
      or  lower(emar.medication) like '%amikin%' 
      or  lower(emar.medication) like '%amoxicill%' 
      or  lower(emar.medication) like '%ancef%' 
      or  lower(emar.medication) like '%clavulanate%' 
      or  lower(emar.medication) like '%ampicillin%' 
      or  lower(emar.medication) like '%augmentin%' 
      or  lower(emar.medication) like '%avelox%' 
      or  lower(emar.medication) like '%avidoxy%' 
      or  lower(emar.medication) like '%azactam%' 
      or  lower(emar.medication) like '%azithromycin%' 
      or  lower(emar.medication) like '%aztreonam%' 
      or  lower(emar.medication) like '%axetil%' 
      or  lower(emar.medication) like '%bactocill%' 
      or  lower(emar.medication) like '%bactrim%' 
      or  lower(emar.medication) like '%bactroban%' 
      or  lower(emar.medication) like '%bethkis%' 
      or  lower(emar.medication) like '%biaxin%' 
      or  lower(emar.medication) like '%bicillin l-a%' 
      or  lower(emar.medication) like '%cayston%' 
      or  lower(emar.medication) like '%cefazolin%' 
      or  lower(emar.medication) like '%cedax%' 
      or  lower(emar.medication) like '%cefoxitin%' 
      or  lower(emar.medication) like '%ceftazidime%' 
      or  lower(emar.medication) like '%cefaclor%' 
      or  lower(emar.medication) like '%cefadroxil%' 
      or  lower(emar.medication) like '%cefdinir%' 
      or  lower(emar.medication) like '%cefditoren%' 
      or  lower(emar.medication) like '%cefepime%' 
      or  lower(emar.medication) like '%cefotan%' 
      or  lower(emar.medication) like '%cefotetan%' 
      or lower(emar.medication) like '%cefotaxime%' 
      or lower(emar.medication) like '%ceftaroline%' 
      or lower(emar.medication) like '%cefpodoxime%' 
      or lower(emar.medication) like '%cefpirome%' 
      or lower(emar.medication) like '%cefprozil%' 
      or lower(emar.medication) like '%ceftibuten%' 
      or lower(emar.medication) like '%ceftin%' 
      or lower(emar.medication) like '%ceftriaxone%' 
      or lower(emar.medication) like '%cefuroxime%' 
      or lower(emar.medication) like '%cephalexin%' 
      or lower(emar.medication) like '%cephalothin%' 
      or lower(emar.medication) like '%cephapririn%' 
      or lower(emar.medication) like '%chloramphenicol%' 
      or lower(emar.medication) like '%cipro%' 
      or lower(emar.medication) like '%ciprofloxacin%' 
      or lower(emar.medication) like '%claforan%' 
      or lower(emar.medication) like '%clarithromycin%' 
      or lower(emar.medication) like '%cleocin%' 
      or lower(emar.medication) like '%clindamycin%' 
      or lower(emar.medication) like '%cubicin%' 
      or lower(emar.medication) like '%dicloxacillin%' 
      or lower(emar.medication) like '%dirithromycin%' 
      or lower(emar.medication) like '%doryx%' 
      or lower(emar.medication) like '%doxycy%' 
      or lower(emar.medication) like '%duricef%' 
      or lower(emar.medication) like '%dynacin%' 
      or lower(emar.medication) like '%ery-tab%' 
      or lower(emar.medication) like '%eryped%' 
      or lower(emar.medication) like '%eryc%' 
      or lower(emar.medication) like '%erythrocin%' 
      or lower(emar.medication) like '%erythromycin%' 
      or lower(emar.medication) like '%factive%' 
      or lower(emar.medication) like '%flagyl%' 
      or lower(emar.medication) like '%fortaz%' 
      or lower(emar.medication) like '%furadantin%' 
      or lower(emar.medication) like '%garamycin%' 
      or lower(emar.medication) like '%gentamicin%' 
      or lower(emar.medication) like '%kanamycin%' 
      or lower(emar.medication) like '%keflex%' 
      or lower(emar.medication) like '%kefzol%' 
      or lower(emar.medication) like '%ketek%' 
      or lower(emar.medication) like '%levaquin%' 
      or lower(emar.medication) like '%levofloxacin%' 
      or lower(emar.medication) like '%lincocin%' 
      or lower(emar.medication) like '%linezolid%' 
      or lower(emar.medication) like '%macrobid%' 
      or lower(emar.medication) like '%macrodantin%' 
      or lower(emar.medication) like '%maxipime%' 
      or lower(emar.medication) like '%mefoxin%' 
      or lower(emar.medication) like '%metronidazole%' 
      or lower(emar.medication) like '%meropenem%' 
      or lower(emar.medication) like '%methicillin%' 
      or lower(emar.medication) like '%minocin%' 
      or lower(emar.medication) like '%minocycline%' 
      or lower(emar.medication) like '%monodox%' 
      or lower(emar.medication) like '%monurol%' 
      or lower(emar.medication) like '%morgidox%' 
      or lower(emar.medication) like '%moxatag%' 
      or lower(emar.medication) like '%moxifloxacin%' 
      or lower(emar.medication) like '%mupirocin%' 
      or lower(emar.medication) like '%myrac%' 
      or lower(emar.medication) like '%nafcillin%' 
      or lower(emar.medication) like '%neomycin%' 
      or lower(emar.medication) like '%nicazel doxy 30%' 
      or lower(emar.medication) like '%nitrofurantoin%' 
      or lower(emar.medication) like '%norfloxacin%' 
      or lower(emar.medication) like '%noroxin%' 
      or lower(emar.medication) like '%ocudox%' 
      or lower(emar.medication) like '%ofloxacin%' 
      or lower(emar.medication) like '%omnicef%' 
      or lower(emar.medication) like '%oracea%' 
      or lower(emar.medication) like '%oraxyl%' 
      or lower(emar.medication) like '%oxacillin%' 
      or lower(emar.medication) like '%pc pen vk%' 
      or lower(emar.medication) like '%pce dispertab%' 
      or lower(emar.medication) like '%panixine%' 
      or lower(emar.medication) like '%pediazole%' 
      or lower(emar.medication) like '%penicillin%' 
      or lower(emar.medication) like '%periostat%' 
      or lower(emar.medication) like '%pfizerpen%' 
      or lower(emar.medication) like '%piperacillin%' 
      or lower(emar.medication) like '%tazobactam%' 
      or lower(emar.medication) like '%primsol%' 
      or lower(emar.medication) like '%proquin%' 
      or lower(emar.medication) like '%raniclor%' 
      or lower(emar.medication) like '%rifadin%' 
      or lower(emar.medication) like '%rifampin%' 
      or lower(emar.medication) like '%rocephin%' 
      or lower(emar.medication) like '%smz-tmp%' 
      or lower(emar.medication) like '%septra%' 
      or lower(emar.medication) like '%septra ds%' 
      or lower(emar.medication) like '%septra%' 
      or lower(emar.medication) like '%solodyn%' 
      or lower(emar.medication) like '%spectracef%' 
      or lower(emar.medication) like '%streptomycin%' 
      or lower(emar.medication) like '%sulfadiazine%' 
      or lower(emar.medication) like '%sulfamethoxazole%' 
      or lower(emar.medication) like '%trimethoprim%' 
      or lower(emar.medication) like '%sulfatrim%' 
      or lower(emar.medication) like '%sulfisoxazole%' 
      or lower(emar.medication) like '%suprax%' 
      or lower(emar.medication) like '%synercid%' 
      or lower(emar.medication) like '%tazicef%' 
      or lower(emar.medication) like '%tetracycline%' 
      or lower(emar.medication) like '%timentin%' 
      or lower(emar.medication) like '%tobramycin%' 
      or lower(emar.medication) like '%trimethoprim%' 
      or lower(emar.medication) like '%unasyn%' 
      or lower(emar.medication) like '%vancocin%' 
      or lower(emar.medication) like '%vancomycin%' 
      or lower(emar.medication) like '%vantin%' 
      or lower(emar.medication) like '%vibativ%' 
      or lower(emar.medication) like '%vibra-tabs%' 
      or lower(emar.medication) like '%vibramycin%' 
      or lower(emar.medication) like '%zinacef%' 
      or lower(emar.medication) like '%zithromax%' 
      or lower(emar.medication) like '%zosyn%' 
      or lower(emar.medication) like '%zyvox%' )
and event_txt in ("Administered", "Started")
  ) emar
ON pdx.hadm_id = emar.hadm_id
where datetime_diff(emar.charttime, intime, minute) > -48*60 
   and datetime_diff(emar.charttime, intime, minute) - eligibility_time <= 96*60

) 
tmp left join pdx on pdx.stay_id = tmp.stay_id
where variable is not null and value is not null
and (pdx.imv = 0 or tmp.time <= pdx.imv_time)
group by 1,2,3
order by 1,2