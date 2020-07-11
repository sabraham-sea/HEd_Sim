-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

select usr.firstname,usr.lastname,sch.id as school_id, prv.id as provider_id,cap.id as cap_id,sch.slug
from he_tables.school as sch
left join he_tables.user  as usr
on sch.account_manager_id = usr.id
left join he_tables.school_provider as prv
on sch.id = prv.school_id
left join he_tables.school_provider_cap as cap
on prv.id = cap.provider_id
where usr.is_enabled = 1