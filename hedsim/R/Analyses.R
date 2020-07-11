require(fpp3)
library(ggplot2)
library(tsibble)
library(feasts)

connection_string <- get_environment_variable("CONNECTION_STRING_HED_REDSHIFT")
connection <- get_connection(connection_string)

# CDM_traffic / SUBJECT NOT AS POPULATED
query_statement1 <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_traffic.sql")
CDM_traffic <- DBI::dbGetQuery(connection, query_statement1)
## Clean up traffic
CDM_traffic$subject_name<-if_else(is.na(CDM_traffic$subject_name),CDM_traffic$category_name,CDM_traffic$subject_name)
CDM_traffic%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%group_by(yr)%>%skim()
CDM_traffic%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%group_by(yr)%>%select(subject_name,category_name,yr)%>%skim()

# CDM APPviews
query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_appview_refresh.sql")
CDM_appview <- DBI::dbGetQuery(connection, query_statement)
CDM_appview_yr<-CDM_appview%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%select(-date)


#Legacy traffic  /CAP ID ISSUE ## CANNOT TIE BACK TO CAP-SCHOOL-DCS
query_statement1 <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/legacy_traffic.sql")
legacy_traffic <- DBI::dbGetQuery(connection, query_statement1)
yearly_traffic<-legacy_traffic%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%group_by(yr)%>%summarise(traffic=sum(traffic))
monthly_traffic<-legacy_traffic%>%mutate(yrmnth=yearmonth(date),yr=year(date),mnth=month(date))%>%group_by(yr,mnth)%>%summarise(traffic=sum(traffic))

#### Make timeseries ###

#Daily TS
legacy_traffic %>% mutate(yrmnth=yearmonth(date),yr=year(date))%>%
select(-date)%>%
as_tsibble(key = yr, index = yrmnth)%>%autoplot(traffic)+ggtitle("Overall Legacy Traffic w/ daily datapoints")

#Monthly
##
monthly_traffic%>%
  as_tsibble(key = yr, index = mnth)%>%autoplot(traffic) +xlab("mnth")+ggtitle("Overall Year Comparison ")

##
k1<-legacy_traffic %>% mutate(yrmnth=zoo::as.yearmon(date))%>%
  select(-date)%>%group_by(yrmnth)%>%summarise(tottraf=sum(traffic))%>%ungroup()%>%
  mutate(yrmnth1=yearmonth(yrmnth),yr=year(yrmnth))%>%
  as_tsibble(key = yr, index = yrmnth1)%>%autoplot(tottraf)+ggtitle("Legacy Traffic w/ monthly datapoints")


########## Legacy appviews ##########

query_statement1 <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/legacy_appview.sql")
legacy_appview <- DBI::dbGetQuery(connection, query_statement1)
legacy_appview%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%skim()


# Check institutions-capid that have dropped off

l1<-legacy_appview%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%group_by(yr,cap_id)%>%
  distinct(cap_id,school_name)%>%pivot_wider(names_from =yr, values_from = cap_id)


# Check only 2019

list2019<-legacy_appview%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%group_by(yr,cap_id)%>%filter(yr==2019)%>%
  distinct(cap_id)

#Daily TS


overall_legappview<-legacy_appview %>% mutate(yrmnth=zoo::as.yearmon(date))%>%
  group_by(yrmnth,cap_id,degree_name,school_name,category_name,subject_name)%>%
  summarise(totunits=sum(accepted_units))%>%
  mutate(yr=year(yrmnth),yrmnth1=yearmonth(yrmnth))%>%
  ungroup()%>%
  as_tsibble(key = c(cap_id,school_name,degree_name,category_name,subject_name), index = yrmnth1)

# distinct schoolnames

dist_schoolnames<-legacy_appview%>%filter(year(date)==2019)%>%select(school_name)%>%distinct()

## By school

t<-overall_legappview%>%filter(school_name=="southern-new-hampshire-university")%>%
  group_by(school_name,cap_id,yr)%>%
  summarise(totunits=sum(totunits))%>%ungroup()%>%
  as_tsibble(key = c(cap_id,school_name), index = yrmnth1)


t%>%autoplot(totunits)+ggtitle("Overall appview")
t%>%filter(yr==2018)%>%gg_season(totunits)  # shd have complete years for this



c(yr,cap_id,school_name,degree_name,category_name,subject_name)

########## CURRENT APPVIEW

overall_currappview<-CDM_appview %>% mutate(yrmnth=zoo::as.yearmon(date))%>%
  group_by(yrmnth,cap_id,degree_name,school_name,category_name,subject_name)%>%
  summarise(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),
            cpc_clicks=sum(accepted_cpc_clicks),cpc_leads=sum(accepted_cpc_leads))%>%
  mutate(yr=year(yrmnth),yrmnth1=yearmonth(yrmnth))%>%
  ungroup()%>%
  as_tsibble(key = c(cap_id,school_name,degree_name,category_name,subject_name), index = yrmnth1)


## By school

t<-overall_currappview%>%filter(school_name=="southern-new-hampshire-university")%>%
  group_by(school_name,cap_id,yr)%>%
  summarise(cpl_leads=sum(cpl_leads),cpl_views=sum(cpl_views),
            cpc_clicks=sum(cpc_clicks),cpc_leads=sum(cpc_leads))%>%ungroup()%>%
  as_tsibble(key = c(cap_id,school_name), index = yrmnth1)


t%>%autoplot(cpl_leads)+ggtitle("Overall appview")
t%>%filter(yr==2019)%>%gg_season(cpl_leads)  # shd have complete years for this

## CDM Traffic
CDM_traffic$subject_name<-if_else(is.na(CDM_traffic$subject_name),CDM_traffic$category_name,CDM_traffic$subject_name)


# Counts by cap-school-dcs

overall_currtraffic<-CDM_traffic%>%mutate(yrmnth=zoo::as.yearmon(date),yr=year(date))%>%
  select(-date)%>%group_by(yrmnth,cap_id,school_name,degree_name,category_name,subject_name)%>%ungroup()%>%
  summarise(dcs_trf=sum(dcs_traffic))


t0<-CDM_traffic%>%mutate(yrmnth=zoo::as.yearmon(date),yr=year(date))%>%
  select(-date)%>%group_by(yr,cap_id,school_name,degree_name,category_name,subject_name)%>%
  summarise(dcs_trf=sum(dcs_traffic))


# # Missing Analyses
# ###
# library(mice)
# library(VIM)
#
#
# missing<-t0%>%filter(is.na(subject_name))%>%pivot_wider(names_from = yr, values_from = dcs_trf)%>%ungroup()
# missing$subject_name<-if_else(is.na(missing$subject_name),missing$category_name,missing$subject_name)
#
# missfin<-missing%>%filter(is.na(subject_name))
#
# CDMmis<-md.pattern(missing)
#
# mice_plot <- aggr(CDMmis, col=c('navyblue','yellow'),
#                   numbers=TRUE, sortVars=TRUE,
#                   labels=names(CDMmis), cex.axis=.7,
#                   gap=3, ylab=c("Missing data","Pattern"))
#
# imputed_CDM<-mice(data.frame(missing$subject_name,missing$category_name,missing$cap_id),m=5,maxit=5,method="polyreg")
#
#
# CDM_SNHU<-CDM_traffic%>%filter(school_name=='southern-new-hampshire-university')
# CDMmis<-md.pattern(CDM_SNHU)
#
# mice_plot <- aggr(CDMmis, col=c('navyblue','yellow'),
#                     numbers=TRUE, sortVars=TRUE,
#                     labels=names(CDMmis), cex.axis=.7,
#                     gap=3, ylab=c("Missing data","Pattern"))
#
#
# imputed_CDM<-mice(CDM_traffic,m=5,maxit=5,method="polyreg")
# imputed_CDMdata=complete(imputed_CDM,5)
# densityplot(imputed_CDM)
#
#
# imputed_t0<-mice(data.frame(CDM_SNHU$category_name,CDM_SNHU$subject_name),m=5,maxit=5,method="polyreg")
# imputed_CDMdata=complete(imputed_CDM,5)
# densityplot(imputed_CDM)



t0<-CDM_traffic%>%mutate(yrmnth=zoo::as.yearmon(date),yr=year(date))%>%
  select(-date)%>%group_by(yr,cap_id,school_name,degree_name,category_name,subject_name)%>%
  summarise(dcs_trf=sum(dcs_traffic))

# Counts by cap-school-ds   / remove category

t1<-CDM_traffic%>%mutate(yrmnth=zoo::as.yearmon(date),yr=year(date))%>%
  select(-date)%>%group_by(yr,cap_id,school_name,degree_name,subject_name)%>%
  summarise(dcs_trf=sum(dcs_traffic))

# Counts by cap-ds  / remove school and category

t2<-CDM_traffic%>%mutate(yrmnth=zoo::as.yearmon(date),yr=year(date))%>%
  select(-date)%>%group_by(yr,cap_id,degree_name,subject_name)%>%
  summarise(dcs_trf=sum(dcs_traffic))


# Counts by cap /

t3<-CDM_traffic%>%mutate(yrmnth=zoo::as.yearmon(date),yr=year(date))%>%
  select(-date)%>%group_by(yr,cap_id)%>%
  summarise(dcs_trf=sum(dcs_traffic))


# Find missing subject names

t0_miss_2019<-t0%>%filter(is.na(subject_name) & yr==2019)
t0_miss_2020<-t0%>%filter(is.na(subject_name) & yr==2020)
#### Make timeseries ###

#Daily TS
CDM_traffic %>% mutate(yrmnth=yearmonth(date),yr=year(date))%>%
  select(-date)%>%
  as_tsibble(key=c(cap_id,school_name,degree_name,category_name,subject_name), index = yrmnth)%>%autoplot(dcs_traffic)+ggtitle("Overall CDM Traffic w/ daily datapoints")


##
k1<-CDM_traffic %>% mutate(yrmnth=zoo::as.yearmon(date))%>%
  select(-date)%>%group_by(yrmnth)%>%summarise(tottraf=sum(dcs_traffic))%>%ungroup()%>%
  mutate(yrmnth1=yearmonth(yrmnth),yr=year(yrmnth))%>%
  as_tsibble(key = yr, index = yrmnth1)%>%autoplot(tottraf)+ggtitle("CDM Traffic w/ monthly datapoints")
########################################################################################################################

#########################################################################################################################
# Combine traffic-views : aggregated

currappview<-CDM_appview %>% mutate(yrmnth=zoo::as.yearmon(date))%>%
  group_by(yrmnth,cap_id,degree_name,school_name,category_name,subject_name)%>%
  summarise(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),
            cpc_clicks=sum(accepted_cpc_clicks),cpc_leads=sum(accepted_cpc_leads),
            accepted_revenue=sum(accepted_revenue),billable_revenue=sum(billable_revenue),
            revised_revenue=sum(revised_revenue)
            )%>%
  mutate(yr=year(yrmnth),yrmnth1=yearmonth(yrmnth))%>%
  ungroup()

currtraffic<-CDM_traffic%>%mutate(yrmnth=zoo::as.yearmon(date),yr=year(date))%>%
  select(-date)%>%group_by(yrmnth,cap_id,school_name,degree_name,subject_name)%>%
  summarise(dcs_trf=sum(dcs_traffic))%>%ungroup()

combined_trf_app<-left_join(currappview,currtraffic,by=c("yrmnth","cap_id","school_name","degree_name","subject_name"))

#Impute dcs_traffic

require(mice)
require(VIM)
traffic_missing<-md.pattern(combined_trf_app)
mice_plot <- aggr(traffic_missing, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(traffic_missing), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

imputed_CDM<-mice(data.frame(combined_trf_app$dcs_trf,combined_trf_app$cap_id),m=5,maxit=5,method="pmm")
imputed_CDMdata=complete(imputed_CDM,5)
densityplot(imputed_CDM)

#Imputed monthly data
trf_new<-cbind(imputed_CDMdata$combined_trf_app.dcs_trf ,combined_trf_app)
saveRDS(trf_new,"trf_new.RDS")

######## Combine traffic-views : daily : USE THIS

CDM_traffic_clean<-CDM_traffic%>%filter(cap_id != '')
CDM_appview_clean<-CDM_appview%>%filter(cap_id!='')

combined_daily<-left_join(CDM_appview_clean,CDM_traffic_clean,by=c("date","cap_id","school_name","degree_name","subject_name"))
combined_daily_inner<-inner_join(CDM_appview_clean,CDM_traffic_clean,by=c("date","cap_id","school_name","degree_name","subject_name"))

traffic_missing<-md.pattern(combined_daily)
mice_plot <- aggr(traffic_missing, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(traffic_missing), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

imputed_daily<-mice(data.frame(combined_daily$dcs_traffic,combined_daily$cap_id),m=5,maxit=5,method="pmm")
daily_new=complete(imputed_daily,5)
densityplot(imputed_CDM)

trf_daily_final<-cbind(daily_new$combined_daily.dcs_traffic  ,combined_daily)



saveRDS(trf_daily_final,"trf_daily_final.RDS")



cc<-combined_daily%>%filter(cap_id!='NA')

cc1<-cc%>%filter(cap_id == '965' & date =='2020-05-18')
