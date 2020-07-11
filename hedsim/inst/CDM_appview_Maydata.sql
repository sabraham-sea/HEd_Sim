SELECT
    ot.cap_id,
    prov.id                   as provider_id,
    prov.school_id    as school_id,
    prov.name                 as provider,
    date,
    spc.name                  as cap_name,
    ot.school_name,
    ot.degree_name,
    ot.category_name,
    ot.subject_name,

      ot.accepted_probability,
       ot.accepted_revenue,
       ot.revised_probability,
       ot.revised_revenue,
       ot.billable_probability,
       ot.billable_revenue,
sum(case when msg_outcomevaluestring = 'exclusive-cpl' and msg_outcometype_category = 'LEAD' then accepted_probability else 0 end) as accepted_cpl_leads,
sum(case when msg_outcomevaluestring = 'exclusive-cpl' and msg_outcometype_category = 'APP_VIEWED' then accepted_probability else 0 end) as accepted_cpl_appviews,
sum(case when msg_outcomevaluestring = 'exclusive-cpc' and msg_outcometype_category = 'CLICK' then accepted_probability else 0 end) as accepted_cpc_clicks,
sum(case when msg_outcomevaluestring = 'exclusive-cpc' and msg_outcometype_category = 'LEAD' then accepted_probability else 0 end) as accepted_cpc_leads
FROM higher_education_cdm.conversion AS OT
LEFT JOIN dev.he_tables.school_provider_cap AS spc
    ON spc.id = OT.cap_id
LEFT JOIN dev.he_tables.school_provider AS prov
    ON prov.id = spc.provider_id
    where date >= '2020-05-01' 
GROUP BY 1, 2,3, 4, 5,6,7,8,9,10,11,12,13,14,15,16
order by 4

