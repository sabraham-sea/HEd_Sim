-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

SELECT
   to_char(date,'Month') as mnth,
    to_char(date,'YYYY') as yr,
       COUNT (DISTINCT msg_viewcorrelationid ) AS dcs_traffic
from higher_education_cdm.product AS product
group by 1,2
order by  2
