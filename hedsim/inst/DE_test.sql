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
                def.school_capid,
                def.school_name,
                COUNT(def.decisions_requested) AS Requested_Cnt,
                COUNT(def.decisions_created) AS Created_Cnt,
                COUNT(def.decisions_provided) AS Provided_Cnt,
                AVG(def.total_erpv) AS avg_erpi,
                AVG(def.ctr) AS avg_ctr,
                AVG(def.converstion_rate) AS avg_cr,
                AVG(def.strategic_lever) AS avg_sl,
                AVG(def.prerank) AS avg_prerank,
                AVG(def.postrank) AS avg_postrank,
                AVG(def.finalrank) AS avg_finalrank

FROM he_tables.decisionengine_fact AS def
WHERE def.date > current_date - 15 and def.school_capid ='1014'
GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13
