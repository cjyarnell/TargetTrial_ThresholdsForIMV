with tm as (SELECT
    n.admissionid,
    (measuredat - a.admittedat)/(1000*60) AS chartminute,
    case when n.itemid IN (6642, 6679, 8843) then "mbp"
         when n.itemid = 6640 then "heart_rate"
         when n.itemid = 6709 then "spo2"
         end as item,
    n.value,
    CASE
        WHEN NOT registeredby IS NULL THEN TRUE
        ELSE FALSE
    END as validated
FROM `alpine-scholar-292916.Amsterdam.numericitems` n
LEFT JOIN `alpine-scholar-292916.Amsterdam.admissions` a ON
n.admissionid = a.admissionid
WHERE itemid IN (
    6640, --Hartfrequentie
    6642, --ABP gemiddeld
    6679, --Niet invasieve bloeddruk gemiddeld
    8843, --ABP gemiddeld II
    6709 -- spo2
)

AND (measuredat - a.admittedat) <= 1000*60*60*144 --measurements within 144 hours
AND (measuredat - a.admittedat) >= -1000*60*60*48 --measurements within 48h prior hours
)

select * from tm where validated = TRUE order by 1,2,3