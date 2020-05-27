-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

SELECT
   date ,
       degree_name,
       category_name,
       subject_name,
    school_slug as school_name,
    product_cap_id as cap_id,
    msg_outcomevaluestring,
    sum(case when msg_outcomevaluestring = 'exclusive-cpl' then accepted_leads
           when msg_outcomevaluestring = 'exclusive-cpc' then accepted_form_views
           else 0 end) as accepted_units
  FROM he_tables.form f
where msg_outcomevaluestring != 'select-cpl'
--and f.school_slug = 'southern-new-hampshire-university'
  GROUP BY 1,2,3,4,5,6,7
