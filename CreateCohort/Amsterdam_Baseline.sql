-- query to make baseline characteristics table for AMDS data

with tm as (
  select
  *
  from `alpine-scholar-292916.PAHRC1.AMDS_TT_Eligibility`
  where operative = 0 
  and eligibletime < 1440
  and o2_device in (16,17)
  and (goc is null or goc = 1 or goc = 2)
), 

tv as (

select distinct * from (

select
    tm.admissionid
  , o2.chartminute-eligibletime as chartminute
  , "o2_device" as var
  , o2.o2_device as value
from tm 
left join `alpine-scholar-292916.Amsterdam.derived_fio2_o2device` o2 
on tm.admissionid = o2.admissionid
where (lvalidated = TRUE or nvalidated = TRUE)

union all

-- intubated during observation period?
select
    tm.admissionid
  , o2.chartminute-eligibletime as chartminute
  , "imv_obs" as var
  , max(case when o2.o2_device = 20 then 1 else 0 end) over 
        (partition by tm.admissionid order by o2.chartminute range between current row and 5760 following) as value
from tm 
left join `alpine-scholar-292916.Amsterdam.derived_fio2_o2device` o2 
on tm.admissionid = o2.admissionid
where (lvalidated = TRUE or nvalidated = TRUE)

union all

-- if intubated during observation period, time of that event
select
    tm.admissionid
  , o2.chartminute-eligibletime as chartminute
  , "imv_time" as var
  , min(case when o2.o2_device = 20 then chartminute-eligibletime end) over 
        (partition by tm.admissionid order by o2.chartminute range between current row and 5760 following) as value
from tm 
left join `alpine-scholar-292916.Amsterdam.derived_fio2_o2device` o2 
on tm.admissionid = o2.admissionid
where (lvalidated = TRUE or nvalidated = TRUE)

union all

-- intubated ever within 28 days?
select
    tm.admissionid
  , o2.chartminute-eligibletime as chartminute
  , "imv" as var
  , max(case when o2.o2_device = 20 then 1 else 0 end) over 
        (partition by tm.admissionid order by o2.chartminute range between current row and 40320 following) as value
from tm 
left join `alpine-scholar-292916.Amsterdam.derived_fio2_o2device` o2 
on tm.admissionid = o2.admissionid
where (lvalidated = TRUE or nvalidated = TRUE)

union all

-- if intubated ever within 28d, time of that event
select
    tm.admissionid
  , o2.chartminute-eligibletime as chartminute
  , "imv_ever_time" as var
  , min(case when o2.o2_device = 20 then chartminute-eligibletime end) over 
        (partition by tm.admissionid order by o2.chartminute range between current row and 40320 following) as value
from tm 
left join `alpine-scholar-292916.Amsterdam.derived_fio2_o2device` o2 
on tm.admissionid = o2.admissionid
where (lvalidated = TRUE or nvalidated = TRUE)

union all


select
    tm.admissionid
  , o2.chartminute-eligibletime as chartminute
  , "fio2" as var
  , fio2 as value
from tm 
left join `alpine-scholar-292916.Amsterdam.derived_fio2_o2device` o2 
on tm.admissionid = o2.admissionid
where (lvalidated = TRUE or nvalidated = TRUE)

UNION ALL 

select
    tm.admissionid
  , chartminute-eligibletime as chartminute
  , "resp_rate" as var
  , value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_RR` rr
on rr.admissionid = tm.admissionid
where rr.validated = TRUE

UNION ALL 

select
    tm.admissionid
  , chartminute-eligibletime as chartminute
  , vs.item as var
  , vs.value as value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_vitalsigns_2` vs
on vs.admissionid = tm.admissionid

UNION ALL 

select
    tm.admissionid
  , chartminute-eligibletime as chartminute
  , "GCS_motor" as var
  , motor_score as value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_gcs` vs
on vs.admissionid = tm.admissionid

UNION ALL 

select
    tm.admissionid
  , chartminute-eligibletime as chartminute
  , "GCS_eyes" as var
  , eyes_score as value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_gcs` vs
on vs.admissionid = tm.admissionid

UNION ALL 

select
    tm.admissionid
  , chartminute-eligibletime as chartminute
  , "GCS_verbal" as var
  , verbal_score as value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_gcs` vs
on vs.admissionid = tm.admissionid

UNION ALL 

select
    tm.admissionid
  , chartminute-eligibletime as chartminute
  , "po2" as var
  , po2 as value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_bloodgas` vs
on vs.admissionid = tm.admissionid

UNION ALL 

select
    tm.admissionid
  , chartminute-eligibletime as chartminute
  , "pco2" as var
  , pco2 as value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_bloodgas` vs
on vs.admissionid = tm.admissionid

UNION ALL 

select
    tm.admissionid
  , chartminute-eligibletime as chartminute
  , "ph" as var
  , ph as value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_bloodgas` vs
on vs.admissionid = tm.admissionid

UNION ALL 

select 
    tm.admissionid
  , start/60000-eligibletime as chartminute
  , case when ni.item is not null then ni.item else null end as var
  , case when ni.item is not null then 1 else null end as value
from tm
left join `alpine-scholar-292916.Amsterdam.drugitems` ni
  on ni.admissionid = tm.admissionid
left join `alpine-scholar-292916.Amsterdam.admissions` ad
  on ad.admissionid = ni.admissionid
where 
    ni.itemid IN (7229 -- norepi
                , 7179 -- dopa
                , 6818 -- epi
                , 9126 -- phenyl (only 5...)
    )
and ((start - ad.admittedat)/60000) < 7200

union all

select 
    tm.admissionid
  , stop/60000-eligibletime as chartminute
  , case when ni.item is not null then ni.item else null end as var
  , case when ni.item is not null then -1 else null end as value
from tm
left join `alpine-scholar-292916.Amsterdam.drugitems` ni
  on ni.admissionid = tm.admissionid
left join `alpine-scholar-292916.Amsterdam.admissions` ad
  on ad.admissionid = ni.admissionid
where 
    ni.itemid IN (7229 -- norepi
                , 7179 -- dopa
                , 6818 -- epi
                , 9126 -- phenyl (only 5...)
    )
and ((stop - ad.admittedat)/60000) < 7200

UNION ALL 

select
    tm.admissionid
  , time-eligibletime as chartminute
  , var
  , vs.value as value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_basiclabs` vs
on vs.admissionid = tm.admissionid

UNION ALL 

select
    tm.admissionid
  , time-eligibletime as chartminute
  , var
  , vs.value as value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_basicprocedures` vs
on vs.admissionid = tm.admissionid

UNION ALL 

select
    tm.admissionid
  , time-eligibletime as chartminute
  , var
  , vs.value as value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_basicdrugs` vs
on vs.admissionid = tm.admissionid


)
where chartminute is not null
order by 1, 2, 3
)

select 
  rownums.* 
, ad.location
, ad.origin
, ad.admissionyeargroup
, ad.dischargedat/60000 - tm.admittedat - tm.eligibletime as dischargeminute
, ad.dateofdeath/60000-tm.admittedat - tm.eligibletime as dateofdeathminute
, ad.destination
, ad.gender
, ad.agegroup
, ad.weightgroup
, ad.heightgroup
, ad.specialty
, ad.admittedat/60000 as admittedatminute  
, ad.admittedat/60000 + tm.eligibletime as eligibleminute
from (
  select 
  *,
  row_number() over (partition by admissionid, var order by chartminute desc) rn
  from tv
  where chartminute <= 0 and value is not null) rownums
left join `alpine-scholar-292916.Amsterdam.admissions` ad
on rownums.admissionid = ad.admissionid
left join tm
on rownums.admissionid = tm.admissionid
where rownums.rn = 1
order by admissionid, var
