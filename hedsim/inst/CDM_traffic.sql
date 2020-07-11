-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

SELECT
   date ,
       first_cap_id as cap_id,first_school as school_name,first_degree as degree_name,first_category as category_name,
       first_subject as subject_name,COUNT ( DISTINCT msg_viewcorrelationid ) AS dcs_traffic
from higher_education_cdm.product AS product
where date <=  current_date
group by 1,2,3,4,5,6
order by  2
