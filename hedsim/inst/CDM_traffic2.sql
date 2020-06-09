-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

SELECT
   to_char(date,'Month') as mnth,
    to_char(date,'YYYY') as yr,
       first_cap_id as cap_id,first_school as school_name,first_degree as degree_name,COUNT ( DISTINCT msg_viewcorrelationid ) AS dcs_traffic
from higher_education_cdm.product AS product
group by 1,2,3,4,5
order by  2
