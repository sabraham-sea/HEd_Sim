
#  Get appview data 


# CDM APPviews
query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_appview_refresh.sql")
CDM_appview <- DBI::dbGetQuery(connection, query_statement)

# Get new tav

query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/DE_ctr.sql")
DE_ctr<- DBI::dbGetQuery(connection, query_statement)


Comb_data <-inner_join(CDM_appview,DE_ctr,by=c("date","school_id","provider_id","degree_name","category_name","subject_name"))

CDM_traffic_clean<-CDM_traffic%>%filter(cap_id != '')
CDM_appview_clean<-CDM_appview%>%filter(cap_id!='')


combined_daily_inner<-inner_join(CDM_appview_clean,CDM_traffic_clean,by=c("date","cap_id","school_name","degree_name","category_name","subject_name"))


# Check 1014


 tt<-combined_daily_inner%>%filter(cap_id=='1014')
tt1<-tt%>%filter(date>='2020-05-01' & date <= '2020-05-30')


#

pp<-CDM_appview_clean%>%filter(cap_id=='1014')
pp1<-pp%>%filter(date>='2020-05-01' & date <= '2020-05-30')