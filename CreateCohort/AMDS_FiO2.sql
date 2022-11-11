-- based on https://github.com/AmsterdamUMC/AmsterdamUMCdb/blob/59bf4e9969dbb62de942d08574d814feff2b66a5/amsterdamumcdb/sql/common/pO2_FiO2_estimated.sql

with tm as (
    SELECT n.admissionid,
        (n.measuredat-a.admittedat)/60000 as chartminute,
--        l.value AS O2_device,
        CASE
/*            WHEN n.itemid IN (
                --FiO2 settings on respiratory support
                6699, --FiO2 %: setting on Evita ventilator
                12279 --O2 concentratie --measurement by Servo-i/Servo-U ventilator
            ) THEN 20 -- IMV
            */
            WHEN n.itemid IN (
                12369, --SET %O2: used with BiPap Vision ventilator
                16246 --Zephyros FiO2: Non-invasive ventilation
            ) THEN 16 -- NIV
            ELSE l.valueid -- see below for legend
        END AS o2_device,
        CASE
            WHEN n.itemid IN (
                --FiO2 settings on respiratory support
                6699, --FiO2 %: setting on Evita ventilator
                12279, --O2 concentratie --measurement by Servo-i/Servo-U ventilator
                12369, --SET %O2: used with BiPap Vision ventilator
                16246 --Zephyros FiO2: Non-invasive ventilation
            ) THEN
                CASE
                    WHEN NOT n.value IS NULL THEN n.value --use the settings
                    ELSE null
                END
            ELSE -- estimate the FiO2
                CASE
                    WHEN l.valueid IN (
                        2, -- Nasaal
                        7, --O2-bril
                        1, --Diep Nasaal
                        3, --Kapje
                        8, --Kinnebak
                        9, --Nebulizer
                        4, --Kunstneus
                        18, --Spreekcanule
                        19, --Spreekklepje
                        10, --Waterset
                        11, --Trach.stoma
                        13, --Ambu
                        14, --Guedel
                        15, --DL-tube
                        16, --CPAP
                        17 --Non-Rebreathing masker
                    ) and (0.21 + 0.03*n.value) < 0.21 THEN 0.21
                    WHEN l.valueid IN (
                        1, --Diep Nasaal
                        3, --Kapje
                        8, --Kinnebak
                        9, --Nebulizer
                        4, --Kunstneus
                        18, --Spreekcanule
                        19, --Spreekklepje
                        10, --Waterset
                        11, --Trach.stoma
                        13, --Ambu
                        14, --Guedel
                        15, --DL-tube
                        16, --CPAP
                        17 --Non-Rebreathing masker
                    ) and (0.21 + 0.03*n.value) > 1 THEN 1
                    WHEN l.valueid IN (
                        1, --Diep Nasaal
                        3, --Kapje
                        8, --Kinnebak
                        9, --Nebulizer
                        4, --Kunstneus
                        18, --Spreekcanule
                        19, --Spreekklepje
                        10, --Waterset
                        11, --Trach.stoma
                        13, --Ambu
                        14, --Guedel
                        15, --DL-tube
                        16, --CPAP
                        17 --Non-Rebreathing masker
                    ) THEN 0.21 + 0.03*n.value
                    WHEN l.valueid IN (
                        12 --B.Lucht
                    ) THEN 0.21
                ELSE null
            END
        END AS fio2,
        CASE
        WHEN NOT l.registeredby IS NULL THEN TRUE
        ELSE FALSE end as lvalidated,
        CASE
        WHEN NOT n.registeredby IS NULL THEN TRUE
        ELSE FALSE END as nvalidated
    FROM `alpine-scholar-292916.Amsterdam.numericitems` n
    LEFT JOIN `alpine-scholar-292916.Amsterdam.admissions` a ON
        n.admissionid = a.admissionid
    LEFT JOIN `alpine-scholar-292916.Amsterdam.listitems` l ON
        n.admissionid = l.admissionid AND
        n.measuredat = l.measuredat AND
        l.itemid = 8189 -- Toedieningsweg (Oxygen device)
    WHERE
        n.itemid IN (
            --Oxygen Flow settings without respiratory support
            8845, -- O2 l/min
            10387, --Zuurstof toediening (bloed)
            18587, --Zuurstof toediening

            --FiO2 settings on respiratory support
            6699, --FiO2 %: setting on Evita ventilator
            12279, --O2 concentratie --measurement by Servo-i/Servo-U ventilator
            12369, --SET %O2: used with BiPap Vision ventilator
            16246 --Zephyros FiO2: Non-invasive ventilation
        )
    --measurements within 144 hours of ICU stay:
    AND (n.measuredat - a.admittedat) <= 1000*60*60*144 AND (n.measuredat - a.admittedat) >= -48*60*60*1000
    AND n.value > 0 --ignore stand by values from Evita ventilator

union distinct

select
    poi.admissionid,
    (registeredat - ad.admittedat)/60000 as chartminute,
    20 as o2_device,
    null as fio2,
    TRUE as lvalidated,
    TRUE as nvalidated
from `alpine-scholar-292916.Amsterdam.procedureorderitems` poi
left join `alpine-scholar-292916.Amsterdam.admissions` ad
on ad.admissionid = poi.admissionid
where  poi.itemid = 9476

union distinct

select
    pi.admissionid,
    (pi.start - ad.admittedat)/60000 as chartminute,
    case when itemid = 9328 then 20 
         when itemid = 10740 then 16 end as o2_device,
    null as fio2,
    TRUE as lvalidated,
    TRUE as nvalidated
FROM `alpine-scholar-292916.Amsterdam.processitems` pi
left join `alpine-scholar-292916.Amsterdam.admissions` ad
on pi.admissionid = ad.admissionid
where itemid in (9328, 10740)
) 

select * from tm 
where (lvalidated is not null or nvalidated is not null)
and ((fio2 is not null) 
or (o2_device is not null))
order by 1,2
