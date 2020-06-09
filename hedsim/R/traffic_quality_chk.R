may<- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_traffic_check.sql")
may_april <- DBI::dbGetQuery(connection, may)



may_april%>%mutate(yrmnth=zoo::as.yearmon(date),yr=year(date))%>%
  select(-date)%>%group_by(yrmnth)%>%select(category_name,subject_name)%>%
skim()



