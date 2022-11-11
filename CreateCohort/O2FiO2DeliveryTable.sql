-- Pulls together useful respiratory parameters by charttime for:
-- 1) the oxygen delivery device (Trach, IMV, NIV, HFNC, NRB, FM, NP, None)
-- 2) the respiratory rate
-- 3) the oxygen saturation (spo2)
-- 4) the fraction of inspired oxygen

WITH tm AS
(
  SELECT stay_id, charttime
  FROM `physionet-data.mimic_derived.ventilator_setting`
  UNION DISTINCT
  SELECT stay_id, charttime
  FROM `physionet-data.mimic_derived.oxygen_delivery`
  union distinct
  select stay_id, charttime
  from `physionet-data.mimic_derived.vitalsign` 
  union distinct
  select stay_id, charttime
  from `physionet-data.mimic_icu.chartevents`
  union distinct
  select stay_id, starttime
  from `physionet-data.mimic_icu.procedureevents`
  where itemid = 224385
)
    SELECT tm.stay_id, tm.charttime
    , max(CASE 
    -- tracheostomy
    WHEN o2_delivery_device_1 IN
    (
        'Tracheostomy tube'
      , 'Trach mask ' -- 16435 observations
    ) or o2_delivery_device_2 IN
    (
        'Tracheostomy tube'
      , 'Trach mask ' -- 16435 observations
    ) or 
    o2_delivery_device_3 IN
    (
        'Tracheostomy tube'
      , 'Trach mask ' -- 16435 observations
    ) or 
    o2_delivery_device_4 IN
    (
        'Tracheostomy tube'
      , 'Trach mask ' -- 16435 observations
    )
        THEN 7 -- trach
    -- mechanical ventilation
    WHEN o2_delivery_device_1 IN
    (
        'Endotracheal tube'
    ) or o2_delivery_device_2 IN
    (
        'Endotracheal tube'
    ) or o2_delivery_device_3 IN
    (
        'Endotracheal tube'
    ) or o2_delivery_device_4 IN
    (
        'Endotracheal tube'
    )
    OR ventilator_mode IN
    (
        '(S) CMV',
        'APRV',
        'APRV/Biphasic+ApnPress',
        'APRV/Biphasic+ApnVol',
        'APV (cmv)',
        'Ambient',
        'Apnea Ventilation',
        'CMV',
        'CMV/ASSIST',
        'CMV/ASSIST/AutoFlow',
        'CMV/AutoFlow',
        'CPAP/PPS',
        'CPAP/PSV+Apn TCPL',
        'CPAP/PSV+ApnPres',
        'CPAP/PSV+ApnVol',
        'MMV',
        'MMV/AutoFlow',
        'MMV/PSV',
        'MMV/PSV/AutoFlow',
        'P-CMV',
        'PCV+',
        'PCV+/PSV',
        'PCV+Assist',
        'PRES/AC',
        'PRVC/AC',
        'PRVC/SIMV',
        'PSV/SBT',
        'SIMV',
        'SIMV/AutoFlow',
        'SIMV/PRES',
        'SIMV/PSV',
        'SIMV/PSV/AutoFlow',
        'SIMV/VOL',
        'SYNCHRON MASTER',
        'SYNCHRON SLAVE',
        'VOL/AC'
    )
    OR ventilator_mode_hamilton IN
    (
        'APRV',
        'APV (cmv)',
        'Ambient',
        '(S) CMV',
        'P-CMV',
        'SIMV',
        'APV (simv)',
        'P-SIMV',
        'VS',
        'ASV'
    ) or pe.itemid = 224385
        THEN 6 --imv
    -- NIV
    WHEN o2_delivery_device_1 IN
    (
        'Bipap mask ', -- 8997 observations
        'CPAP mask ' -- 5568 observations
    ) or o2_delivery_device_2 IN
    (
        'Bipap mask ', -- 8997 observations
        'CPAP mask ' -- 5568 observations
    ) or o2_delivery_device_3 IN
    (
        'Bipap mask ', -- 8997 observations
        'CPAP mask ' -- 5568 observations
    ) or  o2_delivery_device_4 IN
    (
        'Bipap mask ', -- 8997 observations
        'CPAP mask ' -- 5568 observations
    )
    OR ventilator_mode_hamilton IN
    (
        'DuoPaP',
        'NIV',
        'NIV-ST'
    )
        THEN 5 --niv
    -- hfnc
    when o2_delivery_device_1 IN
    (
        'High flow nasal cannula' -- 925 observations
    ) or  o2_delivery_device_2 IN
    (
        'High flow nasal cannula' -- 925 observations
    ) or  o2_delivery_device_3 IN
    (
        'High flow nasal cannula' -- 925 observations
    ) or  o2_delivery_device_4 IN
    (
        'High flow nasal cannula' -- 925 observations
    )
        THEN 4 -- hfnc
    -- non rebreather
    WHEN o2_delivery_device_1 in
    ( 
        'Non-rebreather' -- 5182 observations
        ) or  o2_delivery_device_2 in
    ( 
        'Non-rebreather' -- 5182 observations
        ) or  o2_delivery_device_3 in
    ( 
        'Non-rebreather' -- 5182 observations
        ) or  o2_delivery_device_4 in
    ( 
        'Non-rebreather' -- 5182 observations
        )
        THEN 3 --nrb
    
    -- other oxygen delivery
    WHEN o2_delivery_device_1 in
    (
        
        'Face tent', -- 24601 observations
        'Aerosol-cool', -- 24560 observations
        'Venti mask ', -- 1947 observations
        'Medium conc mask ', -- 1888 observations
 --       'T-piece', -- 1135 observations
        'Ultrasonic neb', -- 9 observations
        'Vapomist', -- 3 observations
        'Oxymizer', -- 1301 observations
        'High flow neb' -- 10785 observations
    ) or  o2_delivery_device_2 in
    (
        
        'Face tent', -- 24601 observations
        'Aerosol-cool', -- 24560 observations
        'Venti mask ', -- 1947 observations
        'Medium conc mask ', -- 1888 observations
 --       'T-piece', -- 1135 observations
        'Ultrasonic neb', -- 9 observations
        'Vapomist', -- 3 observations
        'Oxymizer', -- 1301 observations
        'High flow neb' -- 10785 observations
    ) or      o2_delivery_device_3 in
    (
        
        'Face tent', -- 24601 observations
        'Aerosol-cool', -- 24560 observations
        'Venti mask ', -- 1947 observations
        'Medium conc mask ', -- 1888 observations
 --       'T-piece', -- 1135 observations
        'Ultrasonic neb', -- 9 observations
        'Vapomist', -- 3 observations
        'Oxymizer', -- 1301 observations
        'High flow neb' -- 10785 observations
    ) or  o2_delivery_device_4 in
    (
        
        'Face tent', -- 24601 observations
        'Aerosol-cool', -- 24560 observations
        'Venti mask ', -- 1947 observations
        'Medium conc mask ', -- 1888 observations
 --       'T-piece', -- 1135 observations
        'Ultrasonic neb', -- 9 observations
        'Vapomist', -- 3 observations
        'Oxymizer', -- 1301 observations
        'High flow neb' -- 10785 observations
    )
        THEN 2 --fm
    when o2_delivery_device_1 = 'Nasal cannula' or 
         o2_delivery_device_2 = 'Nasal cannula' or 
         o2_delivery_device_3 = 'Nasal cannula' or 
         o2_delivery_device_4 = 'Nasal cannula'
        then 1 --np
    when o2_delivery_device_1 in 
    (
     'None')
     THEN 0 -- none
    -- not categorized: other
    ELSE NULL END) AS o2_device
    , avg(resp_rate) as resp_rate
    , avg(case when spo2 <= 100 and spo2 >= 5 then spo2 else null end) as spo2
    -- fio2
    
    , avg(case 
    -- first take the charted fio2
    when ce.itemid = 223835 
        then
            case
                when ce.valuenum >= 0.20 and ce.valuenum <= 1
                    then ce.valuenum * 100
                -- improperly input data - looks like O2 flow in litres
                when ce.valuenum > 1 and ce.valuenum < 20
                    then null
                when ce.valuenum >= 20 and ce.valuenum <= 100
                    then ce.valuenum
            ELSE NULL END 
           -- if o2 flow charted and no fio2 charted then use 21 + 3*Flow as in FLORALI
           -- (some use 21 +4*flow but since those formulas are for normal minute ventilation, probably 
           -- best to err on side of a lower increase in fio2 per liter of flow; sicker patients unlikely
           -- to have normal minute ventilation)
           when o2_flow is not null and o2_delivery_device_1 in (
              'Non-rebreather',
              'Nasal cannula', -- 153714 observations
              'Face tent', -- 24601 observations
              'Aerosol-cool', -- 24560 observations
              'Venti mask ', -- 1947 observations
              'Medium conc mask ', -- 1888 observations
      --        'T-piece', -- 1135 observations
              'Ultrasonic neb', -- 9 observations
              'Vapomist', -- 3 observations
              'Oxymizer' -- 1301 observations
              --'High flow neb' -- 10785 observations
           ) and o2_flow > 25 and o2_flow <= 100 then o2_flow -- fio2 mistyped as flow
            when o2_flow is not null and o2_delivery_device_1 in (
              'Non-rebreather',
              'Nasal cannula', -- 153714 observations
              'Face tent', -- 24601 observations
              'Aerosol-cool', -- 24560 observations
              'Venti mask ', -- 1947 observations
              'Medium conc mask ', -- 1888 observations
       --       'T-piece', -- 1135 observations
              'Ultrasonic neb', -- 9 observations
              'Vapomist', -- 3 observations
              'Oxymizer' -- 1301 observations
            --  'High flow neb' -- 10785 observations
              ) and 21+o2_flow*3 <= 100 then 21+o2_flow*3 
           ELSE NULL END) as fio2
  FROM tm
  LEFT JOIN `physionet-data.mimic_derived.ventilator_setting` vs
      ON tm.stay_id = vs.stay_id
      AND tm.charttime = vs.charttime
  LEFT JOIN `physionet-data.mimic_derived.oxygen_delivery` od
      ON tm.stay_id = od.stay_id
      AND tm.charttime = od.charttime
  LEFT JOIN `physionet-data.mimic_derived.vitalsign` vital
      ON tm.stay_id = vital.stay_id
      AND tm.charttime = vital.charttime
  left join `physionet-data.mimic_icu.chartevents` ce
      ON tm.stay_id = ce.stay_id
      AND tm.charttime = ce.charttime
  left join `physionet-data.mimic_icu.procedureevents` pe
      on tm.stay_id = pe.stay_id
      and tm.charttime = pe.starttime
  where
  resp_rate is not null or
  spo2 is not null or
  ventilator_mode is not null or
  ventilator_mode_hamilton is not null or
  o2_delivery_device_1 is not null or
  ce.itemid = 223835 or
  pe.itemid = 224385
  group by 1,2
  order by 1,2
