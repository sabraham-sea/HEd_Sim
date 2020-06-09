SELECT def.year,
                def.month,
                def.date,
                def.msg_experience,
                def.msg_formattype,
                def.msg_degree,
                def.msg_category,
                def.msg_subject,
                def.msg_state,
                def.msg_webcontext_page_url,
                def.publisher,
                cap.school_provider_name,
                def.school_name,
                def.school_capid,
                cap.cap_name,
                cap.price_uom_name,
                COUNT(def.decisions_requested) AS Requested_Cnt,
                COUNT(def.decisions_created) AS Created_Cnt,
                COUNT(def.decisions_provided) AS Provided_Cnt,
                AVG(def.total_erpv) AS avg_erpi,
                AVG(def.ctr) AS avg_ctr,
                AVG(def.converstion_rate) AS avg_cr,
                AVG(def.strategic_lever) AS avg_sl,
                AVG(def.prerank) AS avg_prerank,
                AVG(def.postrank) AS avg_postrank,
                AVG(def.finalrank) AS avg_finalrank,
                AVG(cap.price_amt) AS avg_price,
                AVG(cap.scrub_rate) AS avg_scrub
FROM he_tables.decisionengine_fact AS def
LEFT OUTER JOIN he_tables.cap_info AS cap
ON def.school_capid = cap.cap_id
       AND cap.year = def.year
       AND cap.month = def.month
WHERE def.date > current_date - 15
AND cap.cap_status IN ('ENABLED','enabled','active')
--AND cap.school_name IS NOT NULL
GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
