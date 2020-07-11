select  def.month,
       def.school_capid,
       def.msg_experience,
             def.msg_degree,
                def.msg_category,
                def.msg_subject,
                def.msg_state,
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
AND cap.cap_status IN ('ENABLED','enabled','active')
--AND cap.school_name IS NOT NULL


GROUP BY 1,2,3,4,5,6,7
