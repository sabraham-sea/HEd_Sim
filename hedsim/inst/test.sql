-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

SELECT  sentat as AT TIME ZONE 'Eastern Standard Time'  as sentat_cst

from higher_education_cdm.outcometrackeddetails limit 10


