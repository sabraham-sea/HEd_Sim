-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

SELECT
   date,
    slug,
    msg_outcomevaluestring,
       school_provider_id,
    sum(case when msg_outcomevaluestring = 'exclusive-cpl' and msg_outcometype_category = 'LEAD' then was_clicked else 0 end) as accepted_cpl_leads,
sum(case when msg_outcomevaluestring = 'exclusive-cpl' and msg_outcometype_category = 'APP_VIEWED' then was_clicked else 0 end) as accepted_cpl_appviews,
sum(case when msg_outcomevaluestring = 'exclusive-cpc' and msg_outcometype_category = 'CLICK' then was_clicked else 0 end) as accepted_cpc_clicks,
sum(case when msg_outcomevaluestring = 'exclusive-cpc' and msg_outcometype_category = 'LEAD' then was_clicked else 0 end) as accepted_cpc_leads,
count(was_clicked) as impressions
FROM he_tables.decisionengine_click_through_rate_training
where msg_outcomevaluestring != 'NULL'
GROUP BY 1,2,3,4
order by 2