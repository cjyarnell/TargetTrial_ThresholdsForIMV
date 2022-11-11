-- Amsterdam UMC data

-- timevarying data

with tm as (
  select
  *
  from `alpine-scholar-292916.PAHRC1.AMDS_TT_Eligibility`
  where operative = 0 
  and eligibletime < 1440
  and o2_device in (16,17)
  and (goc is null or goc = 1 or goc = 2)
)

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
  , "GCS" as var
  , gcs_score as value
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
  , time-eligibletime as chartminute
  , var
  , value
from tm
left join `alpine-scholar-292916.Amsterdam.derived_basiclabs` vs
on vs.admissionid = tm.admissionid
where var = "lactate"

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
)
where chartminute is not null
order by 1, 2, 3

