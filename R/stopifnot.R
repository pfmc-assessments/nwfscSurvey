#' Stop if column name is not present in the data
#'
#' Similar in function to [stopifnot] but specific to a column name
#' that you wish to double check if it is present in your data or not.
#'
#' @param data A data frame with column names.
#' @param string A character string that you want to know if it exists in
#' the column names of `data`.
#' @return `TRUE` is invisibly returned if the column name is found and
#' an informative [stop] call is initiated if the column is not present.
#'
stopifnotcolumn <- function(data, string) {
  if (is.na(match(string, colnames(data)))) {
    stop(string, " was not found in your data frame.")
  }
  return(invisible(TRUE))
}
