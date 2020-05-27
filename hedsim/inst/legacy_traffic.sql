-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

select traffic.date,
       sum(monetized_organic_visits) as traffic

from he_tables.traffic as traffic
group by 1
