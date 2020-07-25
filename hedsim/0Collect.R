## Collect Data ##

options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
library(xlsx)
require(rvdata.jdbc)
require(skimr)
require(fpp3)
require(fpp3)
library(ggplot2)
library(tsibble)
library(feasts)
require(mice)
require(VIM)

# Run Utilities and Environment

connection_string <- get_environment_variable("CONNECTION_STRING_HED_REDSHIFT")
connection <- get_connection(connection_string)

# CDM_traffic / SUBJECT NOT AS POPULATED
query_statement1 <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_traffic.sql")
CDM_traffic <- DBI::dbGetQuery(connection, query_statement1)
## Clean up traffic
CDM_traffic$subject_name<-if_else(is.na(CDM_traffic$subject_name),CDM_traffic$category_name,CDM_traffic$subject_name)

# CDM APPviews
query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_appview_refresh_revised.sql")
CDM_appview <- DBI::dbGetQuery(connection, query_statement)

CDM_traffic_clean<-CDM_traffic%>%filter(cap_id != '')
CDM_appview_clean<-CDM_appview%>%filter(cap_id!='')

# combined_daily<-left_join(CDM_appview_clean,CDM_traffic_clean,by=c("date","cap_id","school_name","degree_name","subject_name","category_name"))
# 

month_traffic_org1<-CDM_traffic_clean%>%group_by(date,cap_id,degree_name,subject_name)%>%summarise(tottraff=sum(dcs_traffic))
month_traffic_lat1<-CDM_appview_clean%>%group_by(date,cap_id,provider_id,school_id,school_name,degree_name,subject_name,category_name)%>%summarise(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
                                                                                                 cpc_leads=sum(accepted_cpc_leads))

combined_daily<-left_join(month_traffic_lat1,month_traffic_org1,by=c("cap_id","date","degree_name","subject_name"))
saveRDS(combined_daily,"trf_daily_final_Julynew1.RDS")
saveRDS(CDM_appview_clean,"CDM_appview_clean1.RDS")

#remove(list=ls())
gc()

# To impute  missing traffic
combined_daily<-readRDS("trf_daily_final_Julynew1.RDS")
traffic_missing<-md.pattern(combined_daily)
mice_plot <- aggr(traffic_missing, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(traffic_missing), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

system.time(imputed_daily<-mice(data.frame(combined_daily$tottraff,combined_daily$cap_id),m=5,maxit=5,method="pmm"))
daily_new=complete(imputed_daily,5)
daily_new<-daily_new%>%rename('imptraf'='combined_daily.tottraff','cap_id'='combined_daily.cap_id')
imptraf<-daily_new$imptraf%>%as.data.frame()%>%rename('imptraf'='.')

trf_daily_final<-cbind(combined_daily,imptraf)
#saveRDS(trf_daily_final,"trf_daily_final_July20.RDS")
saveRDS(trf_daily_final,"trf_daily_final_July23.RDS")

# Get ERPI data
query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/DE_MCData3.sql")
DE_MCData3<- DBI::dbGetQuery(connection, query_statement)
saveRDS(DE_MCData3,'DE_MCData3.RDS')

# AM Data
query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/AM_names.sql")
AM_names<- DBI::dbGetQuery(connection, query_statement)
AM_names$cap_id<-as.character(AM_names$cap_id)



# AM Data
query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/capcurrent.sql")
capcurrent<- DBI::dbGetQuery(connection, query_statement)
saveRDS(capcurrent,'capcurrent.RDS')

##############################################################

startdate<- '2020-06-01'
  datechk<-'2020-07-31'


getcpllist<-function(trf_daily_final,DE_MCData3,startdate,datechk)
{
trf_daily_final<-readRDS('trf_daily_final_July14.RDS')
trf_daily_clean<-trf_daily_final
n_traffic<-trf_daily_clean%>%
  filter(date <= datechk )%>% group_by(cap_id)%>%count()%>%filter(n>1)%>%select(cap_id)
overall_cap<-trf_daily_clean%>%filter(date>=startdate & date<=datechk)%>%dplyr::select(cap_id, school_name,date,degree_name,subject_name,cpl_leads,cpl_views,cpc_clicks,cpc_leads,
                                                                        tottraff)%>%
  group_by(cap_id,school_name)%>%
  summarize(cpl_leads=sum(cpl_leads),cpl_views=sum(cpl_views),cpc_clicks=sum(cpc_clicks),
            cpc_leads=sum(cpc_leads),imptrf=sum(tottraff))%>%filter(!is.na(cap_id) & cpl_views>0)%>%select(cap_id)%>%distinct()

pricelist<-CDM_appview_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date>=startdate &  date<= datechk & accepted_revenue>0)%>%
  arrange(cap_id,desc(date))%>%select(-date)%>%distinct()

pricelist<-pricelist[!duplicated(pricelist$cap_id),]

overall_cap<-inner_join(overall_cap,pricelist,by="cap_id")
# Check from erpi data
clean_erpi<- DE_MCData3%>%filter(school_capid != 'NA')%>%rename("cap_id"="school_capid") %>% filter(month==month(startdate))%>%filter(cap_id!='3547') # Problem data point
erpi_caplist<-clean_erpi%>%select(cap_id)%>%distinct()
erpi_caplist$cap_id<-as.character(erpi_caplist$cap_id)
final_list<-inner_join(overall_cap,erpi_caplist,by="cap_id")

return(final_list)
}



final_list<-getcpllist(trf_daily_final,DE_MCData3,'2020-04-30')
saveRDS(final_list,'cpl_listMay.RDS')

final_list<-getcpllist(trf_daily_final,DE_MCData3,'2020-05-01','2020-05-31')
saveRDS(final_list,'cpl_listJune.RDS')

final_list<-getcpllist(trf_daily_final,DE_MCData3,'2020-06-01','2020-06-30')
saveRDS(final_list,'cpl_listJuly.RDS')

final_list<-getcpllist(trf_daily_final,DE_MCData3,'2020-06-01','2020-06-30')
saveRDS(final_list,'cpl_listJuly.RDS')


final_list<-getcpllist(trf_daily_final,DE_MCData3,'2020-07-01','2020-07-31')
saveRDS(final_list,'cpl_listAug.RDS')

getcpclist<-function(trf_daily_final,DE_MCData3,startdate,datechk)
{
  trf_daily_final<-readRDS('trf_daily_final_July12.RDS')
  trf_daily_clean<-trf_daily_final
  n_traffic<-trf_daily_clean%>%
    filter(date <= datechk )%>% group_by(cap_id)%>%count()%>%filter(n>1)%>%select(cap_id)
  overall_cap<-trf_daily_clean%>%filter(date>=startdate & date<=datechk)%>%dplyr::select(cap_id, school_name,date,degree_name,subject_name,cpl_leads,cpl_views,cpc_clicks,cpc_leads,
                                                                                         tottraff)%>%
    group_by(cap_id,school_name)%>%
    summarize(cpl_leads=sum(cpl_leads),cpl_views=sum(cpl_views),cpc_clicks=sum(cpc_clicks),
              cpc_leads=sum(cpc_leads),imptrf=sum(tottraff))%>%filter(!is.na(cap_id) & cpc_clicks>0)%>%select(cap_id)%>%distinct()
  
  pricelist<-CDM_appview_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date>=startdate &  date<= datechk & accepted_revenue>0)%>%
    arrange(cap_id,desc(date))%>%select(-date)%>%distinct()
  
  pricelist<-pricelist[!duplicated(pricelist$cap_id),]
  
  overall_cap<-inner_join(overall_cap,pricelist,by="cap_id")
  # Check from erpi data
  clean_erpi<- DE_MCData3%>%filter(school_capid != 'NA')%>%rename("cap_id"="school_capid") %>% filter(month==month(startdate))%>%filter(cap_id!='3547') # Problem data point
  erpi_caplist<-clean_erpi%>%select(cap_id)%>%distinct()
  erpi_caplist$cap_id<-as.character(erpi_caplist$cap_id)
  final_list<-inner_join(overall_cap,erpi_caplist,by="cap_id")
  
  
  return(final_list)
}


cpc_list<-getcpclist(trf_daily_final,DE_MCData3,'2020-05-01','2020-05-31')
saveRDS(cpc_list,'cpc_listJune.RDS')

cpc_list<-getcpllist(trf_daily_final,DE_MCData3,'2020-06-01','2020-06-30')
saveRDS(final_list,'cpc_listJuly.RDS')

cpc_list<-getcpllist(trf_daily_final,DE_MCData3,'2020-07-01','2020-07-30')
saveRDS(final_list,'cpc_listAug.RDS')
