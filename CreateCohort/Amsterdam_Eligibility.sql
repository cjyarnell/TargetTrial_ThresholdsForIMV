

with admits as (
select
    ad.admissionid
  , case when li.itemid = 18669 then li.value
         when li.itemid = 18671 then li.value 
         else null end as diagnosis
  , case when li.itemid = 18671 and lower(li.value) LIKE "%post-operati%" then 1
         when li.itemid = 18669 and lower(li.value) LIKE "operati%" then 1
         else 0 end as operative
  , case when li.itemid = 18669 and lower(li.value) LIKE "%respirat%" then 1
         when li.itemid = 18671 and lower(li.value) LIKE "%respirat%" then 1
         else 0 end as respiratory
  , case when li.itemid = 18669 and lower(li.value) LIKE "%sepsis%" then 1
         when li.itemid = 18671 and lower(li.value) LIKE "%sepsis%" then 1
         else 0 end as sepsis
  , li.measuredat as charttime
  , urgency
  , admittedat/60000 as admittedat
from 
    `alpine-scholar-292916.Amsterdam.admissions` ad 
left join (
  select * 
  from `alpine-scholar-292916.Amsterdam.listitems` 
  where itemid IN (18669 -- APACHE II dx
                 , 18671 -- APACHE IV dx
                 )) li
on ad.admissionid = li.admissionid
), tm as
(
  select 
    admissionid,
    chartminute,
    last_value(o2_device ignore nulls) over (partition by admissionid order by chartminute range unbounded preceding) as o2_device,
    last_value(fio2 ignore nulls) over (partition by admissionid order by chartminute range 480 preceding) as fio2
  from (select * from `alpine-scholar-292916.Amsterdam.derived_fio2_o2device` where nvalidated = TRUE or lvalidated = TRUE)
), goc as (
  select distinct
    admissionid
  , max(valueid) as goc
  from `alpine-scholar-292916.Amsterdam.derived_goc` 
  where chartminute < 8640
  group by 1
  order by 1
)

select t1.*, goc.goc from (
select distinct
    tm.admissionid
  , tm.chartminute as eligibletime
  , first_value(o2_device) over (partition by tm.admissionid order by charttime range unbounded preceding) as o2_device
  , first_value(operative) over (partition by ad.admissionid order by charttime range unbounded preceding) as operative
  , first_value(respiratory) over (partition by ad.admissionid order by charttime range unbounded preceding) as respiratory
  , first_value(sepsis) over (partition by ad.admissionid order by charttime range unbounded preceding) as sepsis
  , first_value(diagnosis)  over (partition by ad.admissionid order by charttime range unbounded preceding) as diagnosis
  , row_number() over (partition by ad.admissionid order by charttime) as rn
  , ad.admittedat
from admits ad
right join tm
on tm.admissionid = ad.admissionid
where tm.o2_device = 20 
or tm.o2_device in (16,17) and fio2 >= 0.4 and fio2 <= 1
or tm.o2_device in (16,17) and fio2 >= 40 and fio2 <= 100
order by 1,2 ) t1
left join goc on goc.admissionid = t1.admissionid
where rn = 1
