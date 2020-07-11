options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx8192m"))
library(xlsx)
install.packages("xlsx")
library(xlsx)

install.packages("skimr")
require(rvdata.jdbc)
require(skimr)
require(fpp3)

#' #' @title Returns a sql query as a data frame
#' #' @description Initializes connection, reads sql statement and returns result as a data frame
#' #'
#' #'
#' #' @return dataframe
#' #' @import DBI
#'
#' collect_data<-function()
#' {
#'
#'   connection_string <- get_environment_variable("CONNECTION_STRING_HED_REDSHIFT")
#'   connection <- get_connection(connection_string)
#'   sql_statment <- get_sql_statement("CDM_appview.sql")
#'   results <- DBI::dbGetQuery(connection, sql_statment)
#'   DBI::dbDisconnect(connection)
#'   return(results)
#' }

# Collect data


require(dplyr)
require(skimr)

  connection_string <- get_environment_variable("CONNECTION_STRING_HED_REDSHIFT")
  connection <- get_connection(connection_string)

query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_appview_refresh.sql")
CDM_appview <- DBI::dbGetQuery(connection, query_statement)

skim(CDM_appview)


# query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_appview.sql")
# CDM_appview1 <- DBI::dbGetQuery(connection, query_statement)
#
#
# skim(CDM_appview1)

query_statement1 <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_traffic.sql")
CDM_traffic <- DBI::dbGetQuery(connection, query_statement1)
CDM_traffic%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%group_by(yr)%>%skim()
CDM_traffic%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%group_by(yr)%>%select(subject_name,category_name,yr)%>%skim()


# Get more fields

query_statement2 <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_traffic_check.sql")
CDM_traffic_check <- DBI::dbGetQuery(connection, query_statement2)
CDM_traffic_check%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%group_by(yr)%>%select(prodtype,subject_name,yr)%>%skim()


prop.table (table(CDM_traf_chk_prodtype$subject_name,CDM_traf_chk_prodtype$yr,exclude=NULL))

t1<-CDM_traffic_check%>%select(subject_name)%>%table(exclude=NULL)%>%prop.table()%>% {.*100}%>%round(3)%>%as.data.frame()
t2<-CDM_traffic_check%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%select(subject_name,yr)%>%table(exclude=NULL)%>%prop.table()%>% {.*100}%>%round(3)%>%
  as.data.frame()
t3<-t2 %>%
  pivot_wider(names_from = yr, values_from = Freq)

t3%>%filter(is.na(subject_name)| subject_name =='')

t4<-CDM_traffic_check%>%mutate(yrmnth=yearmonth(date),yr=year(date))%>%select(prodtype,subject_name,yr)%>%table(exclude=NULL)%>%prop.table()%>% {.*100}%>%round(3)%>%
  as.data.frame() %>%
  pivot_wider(names_from = yr, values_from = Freq)

t4%>%filter(is.na(subject_name)| subject_name =='')

### Product views

query_statement3 <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_traffic_productviews.sql")
CDM_traffic_prodviews <- DBI::dbGetQuery(connection, query_statement3)

CDM_traffic_prodviews%>%group_by(yr)%>%select(subject_name,category_name,yr)%>%skim()

### Product views

query_statement4 <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_traffic_productclicked.sql")
CDM_traffic_prodclick <- DBI::dbGetQuery(connection, query_statement4)

CDM_traffic_prodclick%>%group_by(yr)%>%select(subject_name,category_name,yr)%>%skim()



query_statement1 <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_traffic.sql")


query_statement2 <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_traffic2.sql")

CDM_traffic2 <- DBI::dbGetQuery(connection, query_statement2)

skim(CDM_traffic2)

overall_traffic<-CDM_traffic2%>%group_by(mnth,yr)%>%summarise(m=sum(dcs_traffic,na.rm=T))

# Looks like category is not getting populated well ; so remove this as a join ?
CDM_traffic%>%group_by(yr)%>%skim()



# Break out traffic data by DCS, where capid and school is missing

traffic_dcs<-CDM_traffic%>%filter(is.na(cap_id)  & is.na(school_name))

traffic_dcs%>%group_by(yr,mnth)%>%skim()

traffic_date<-CDM_traffic%>%filter(is.na(subject_name)  & is.na(category_name)& is.na(degree_name)&is.na(cap_id)  & is.na(school_name))

traffic_capschool<-CDM_traffic%>%filter(is.na(subject_name)  & is.na(category_name)& is.na(degree_name)& !is.na(cap_id)  & !is.na(school_name))

traffic_allpop<-CDM_traffic%>%filter(!is.na(subject_name)  & !is.na(category_name)& !is.na(degree_name)& !is.na(cap_id)  & !is.na(school_name))




# Merge 2

require(dplyr)

CDM_all1 <-left_join(CDM_appview,CDM_traffic, by=c("cap_id","school_name","degree_name","category_name","subject_name","mnth","yr"))

CDM_traffic_clean<-CDM_traffic%>%select(-category_name)
CDM_all2 <-left_join(CDM_appview,CDM_traffic_clean, by=c("cap_id","school_name","degree_name","subject_name","mnth","yr"))


overall_traffic<-CDM_all2%>%group_by(mnth,yr)%>%summarise(m=sum(dcs_traffic,na.rm=T))

CDM_traffic_clean2<-CDM_traffic%>%select(-c(category_name,subject_name))
CDM_all2 <-left_join(CDM_appview,CDM_traffic_clean2, by=c("cap_id","school_name","degree_name","mnth","yr"))

overall_traffic<-CDM_traffic_clean2%>%group_by(mnth,yr)%>%summarise(m=sum(dcs_traffic,na.rm=T))




CDM_all3 <-left_join(CDM_appview,CDM_traffic2, by=c("cap_id","school_name","degree_name","mnth","yr"))

overall_traffic<-CDM_all3%>%group_by(mnth,yr)%>%summarise(m=sum(dcs_traffic,na.rm=T))



query_statement1 <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/legacy_traffic.sql")

legacy_traffic <- DBI::dbGetQuery(connection, query_statement1)

install.packages("fpp3")
require(fpp3)

legacy_traffic<-legacy_traffic%



#########3

connection_string <- get_environment_variable("CONNECTION_STRING_HED_REDSHIFT")
connection <- get_connection(connection_string)

query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/DE_Fact.sql")
DE_fact <- DBI::dbGetQuery(connection, query_statement)


query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/DE_test.sql")
DE_test<- DBI::dbGetQuery(connection, query_statement)

DE_testgp<-DE_test%>%group_by(month)%>%summarise(erpi=mean(avg_erpi,na.rm=TRUE))
skim(DE_fact1)

query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/DE_test.sql")
DE_test<- DBI::dbGetQuery(connection, query_statement)

query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/DE_errchk.sql")
DE_errchk<- DBI::dbGetQuery(connection, query_statement)

skim(DE_errchk)
