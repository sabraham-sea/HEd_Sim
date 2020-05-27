-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

SELECT
   to_char(date,'Month') as mnth,
    to_char(date,'YYYY') as yr,
       first_cap_id,first_school,first_degree,first_category,
       first_subject,COUNT ( DISTINCT msg_viewcorrelationid ) AS dcs_traffic
from higher_education_cdm.product AS product
group by 1,2,3,4,5,6,7
order by  2
