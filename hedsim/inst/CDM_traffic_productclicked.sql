-- !preview conn=DBI::dbConnect(RSQLite::SQLite())


SELECT
      to_char(pv.date,'Month') as mnth,
       to_char(pv.date,'YYYY') as yr,
       msg_product_sku as cap_id,
       msg_product_brand as school_name,
       msg_product_variant as degree_name,
       msg_product_category as category_name,
       msg_product_name as subject_name,COUNT ( DISTINCT msg_viewcorrelationid ) AS dcs_traffic
from higher_education_cdm.productclicked as pv
where date <=  '2020-04-30'
group by 1,2,3,4,5,6,7
order by  2
