

#' @title Returns a sql query as a data frame
#' @description Initializes connection, reads sql statement and returns result as a data frame
#'
#'
#' @return dataframe
#' @import DBI

collect_data<-function()
{

  connection_string <- get_environment_variable("CONNECTION_STRING_HED_REDSHIFT")
  connection <- get_connection(connection_string)
  sql_statment <- get_sql_statement("CDM_appview.sql")
  results <- DBI::dbGetQuery(connection, sql_statment)
  DBI::dbDisconnect(connection)
  return(results)
}

