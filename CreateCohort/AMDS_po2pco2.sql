WITH oxygenation AS (
    SELECT
        pao2.admissionid,
        (pao2.measuredat - a.admittedat)/(1000*60) AS chartminute,
        --pao2.itemid,
        --pao2.item,
        CASE pao2.unitid
            WHEN 152 THEN pao2.value * 7.50061683 -- Conversion: kPa to mmHg
            ELSE pao2.value
        END AS po2,
        CASE paco2.unitid
            WHEN 152 THEN paco2.value * 7.50061683 -- Conversion: kPa to mmHg
            ELSE paco2.value
        END AS pco2,
        ph.value as ph,
        f.value AS specimen_source,
        --pao2.registeredby,
        CASE
            WHEN lower(pao2.registeredby) NOT LIKE '%systeem%' THEN TRUE
            ELSE FALSE
        END AS manual_entry,
        --fio2.itemid,
        --fio2.item,
        --fio2.value AS FiO2,
        --fio2.measuredat,
        --(fio2.measuredat - pao2.measuredat)/(60*1000) AS FiO2_time_difference,
        --ROW_NUMBER() OVER(
        --    PARTITION BY pao2.admissionid, pao2.measuredat
        --    ORDER BY ABS(fio2.measuredat - pao2.measuredat)
        --) AS priority --give priority to nearest FiO2 measurement
    FROM `alpine-scholar-292916.Amsterdam.numericitems` pao2
    LEFT JOIN `alpine-scholar-292916.Amsterdam.admissions` a ON
        pao2.admissionid = a.admissionid
    LEFT JOIN `alpine-scholar-292916.Amsterdam.freetextitems` f ON
        pao2.admissionid = f.admissionid AND
        pao2.measuredat = f.measuredat AND
        f.itemid = 11646 --Afname (bloed): source of specimen
    LEFT JOIN `alpine-scholar-292916.Amsterdam.numericitems` paco2 ON
        pao2.admissionid = paco2.admissionid AND
        pao2.measuredat = paco2.measuredat AND
        paco2.itemid IN (
            6846, --PCO2
            9990, --pCO2 (bloed)
            21213 --PCO2 (bloed) - kPa
        )
    LEFT JOIN `alpine-scholar-292916.Amsterdam.numericitems` ph ON
        pao2.admissionid = ph.admissionid AND
        pao2.measuredat  = ph.measuredat AND
        ph.itemid IN (
    6848, --PH
    12310 --pH (bloed)
    )
    /*LEFT JOIN numericitems fio2 ON
        pao2.admissionid = fio2.admissionid AND
        fio2.itemid IN (
            6699, --FiO2 %: setting on Evita ventilator
            12279, --O2 concentratie --measurement by Servo-i/Servo-U ventilator
            --12282, --O2 concentratie (Set) -- setting on Servo-i/Servo-U ventilator, not needed, same as measurement
            --12329, --O2 concentratie (Set) (2) --setting on 2nd Servo-i/Servo-U ventilator, not needed, same as measurement
            12369, --SET %O2: used with BiPap Vision ventilator
            16246 --Zephyros FiO2: Non-invasive ventilation
        ) AND
        fio2.measuredat > pao2.measuredat - 60*60*1000 AND --no earlier than 60 minutes before pao2 measurement
        fio2.measuredat < pao2.measuredat + 15*60*1000 --no later than 15 minutes after pao2 measurement */
    WHERE
        pao2.itemid IN (
            7433, --PO2
            9996, --PO2 (bloed)
            21214 --PO2 (bloed) - kPa
        )
    --measurements within 48 before / 144 hours after ICU stay :
    AND (pao2.measuredat - a.admittedat) <= 1000*60*60*144 
    AND (pao2.measuredat - a.admittedat) >= -(1000*60*48) 
    AND (lower(f.value) LIKE '%art.%' OR f.value IS NULL)  -- source is arterial or undefined (assume arterial)
)
SELECT * FROM oxygenation
-- WHERE priority = 1
order by 1,2