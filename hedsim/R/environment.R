#' @title Get environment wariable with additional warning handling
#' @description Wraps `Sys.getenv` to get and validate an environment variable.
#'
#' @param variable_name (chr) variable name
#' @param valid_values vector or list. acceptable values for an environment variable
#'
#' @return (chr) Contents of environment variable
get_environment_variable <- function(variable_name, valid_values = NULL) {

  variable_value = Sys.getenv(variable_name)

  if (is.na(variable_value) || is.null(variable_value) || variable_value == "") {
    warning_message <- paste0("Expected environment variable ", variable_name, " is not set.")
    warning(warning_message)
  }

  if (!is.null(valid_values)) {
    match.arg(arg = variable_value, choices = valid_values, several.ok = FALSE)
  }

  return(variable_value)
}

