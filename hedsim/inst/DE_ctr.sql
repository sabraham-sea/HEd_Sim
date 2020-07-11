-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

select dectr.slug AS slug_dectr,
       dectr.school_name as school_name,
       dectr.school_provider_id as provider_id ,
       dectr.de_subject as subject_name,
       dectr.de_degree as degree_name,
       dectr.de_category as category_name,
       dectr.date as date,
sch.slug AS slug_sch,
       schpr.school_id,
sum(was_clicked),count(was_clicked),
sum(case when dectr.msg_outcomevaluestring = 'exclusive-cpl' and dectr.msg_outcometype_category = 'LEAD' then was_clicked else 0 end) as accepted_cpl_leads,
sum(case when dectr.msg_outcomevaluestring = 'exclusive-cpl' and dectr.msg_outcometype_category = 'APP_VIEWED' then was_clicked else 0 end) as accepted_cpl_appviews,
sum(case when dectr.msg_outcomevaluestring = 'exclusive-cpc' and dectr.msg_outcometype_category = 'CLICK' then was_clicked else 0 end) as accepted_cpc_clicks,
sum(case when dectr.msg_outcomevaluestring = 'exclusive-cpc' and dectr.msg_outcometype_category = 'LEAD' then was_clicked else 0 end) as accepted_cpc_leads
from he_tables.decisionengine_click_through_rate_training AS dectr
left join he_tables.school AS sch ON dectr.slug = sch.slug
left join he_tables.school_provider AS schpr ON sch.id = schpr.school_id
group by 1,2,3,4,5,6,7,8,9

