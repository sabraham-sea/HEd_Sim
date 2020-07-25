SELECT cap.cap_id,
       spc.provider_id,
       spc.provider_id  || ' - ' || cap.cap_id AS provider_cap,
       cap.price_uom_name,
       cap.school_name,
       cap.cap_name,
       cap.school_provider_name,
       spc."limit" AS Monthly_Cap_Units,
       ROUND(spc."limit" * (1+spc.scrub_percentage)) AS Allowable_Cap_Units,
       spc."limit" * spc.cpl AS Monthly_Cap_Budget,
       ROUND(spc."limit" * (1+spc.scrub_percentage)) * spc.cpl AS Allowable_Cap_Budget,
       spc.cpl,
       spc.scrub_percentage
FROM he_tables.cap_info AS cap
LEFT OUTER JOIN he_tables.school_provider_cap AS spc
ON cap.cap_id = spc.id AND spc.status = 'ENABLED'
WHERE year = EXTRACT(YEAR FROM current_date)
  AND month = EXTRACT(MONTH FROM current_date)
  AND cap.cap_status IN ('ENABLED','enabled','active')