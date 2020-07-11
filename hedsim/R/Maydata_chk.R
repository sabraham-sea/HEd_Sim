
# Run Utilities and Environment

connection_string <- get_environment_variable("CONNECTION_STRING_HED_REDSHIFT")
connection <- get_connection(connection_string)


# CDM APPviews
query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/CDM_appview_Maydata.sql")
CDM_appview_Maydata <- DBI::dbGetQuery(connection, query_statement)


Maydata<-CDM_appview_Maydata%>%filter(date<='2020-05-31')%>%group_by(cap_id)%>%summarise(totalleads=sum(accepted_cpl_leads))