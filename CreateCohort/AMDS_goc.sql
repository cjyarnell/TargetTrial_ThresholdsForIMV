SELECT 
n.admissionid,
item,
(measuredat-admittedat)/60000 as chartminute,
valueid
FROM `alpine-scholar-292916.Amsterdam.listitems` n
LEFT JOIN `alpine-scholar-292916.Amsterdam.admissions` a ON
n.admissionid = a.admissionid
where item = "Beleid"