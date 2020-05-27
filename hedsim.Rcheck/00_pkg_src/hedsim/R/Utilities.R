#' @title Returns a jdbc connection to redshift
#' @description Returns an active jdbc connection given a connection_string
#'
#' @param connection_string (chr) connection string
#'
#' @return jdbc connection
#' @import rvdata.jdbc
get_connection <- function(connection_string){
  rvdata.jdbc::getRedshiftDBConnection(url=connection_string)
}

#' @title Returns the contents of a sql file within the package
#' @description Returns the contents of a designated file found within the `inst/sql` directory.
#'
#' @param filename (chr) the target file in the `int/sql` directory
#' @return (chr) file contents
#' @import readr
get_sql_statement <- function(filename){
  file_path = system.file("sql", filename,
                          package = "hedsim",
                          mustWork = T)
  query_statement <- readr::read_file(file_path)
  return(query_statement)
}


# file_path = system.file("sql", "CDM_appview.sql",
#                         package = 'hedsim',
#                         mustWork = T)

