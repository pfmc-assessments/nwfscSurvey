#' Reads in the compositional survey data and filters the data into what is necessary
#' It reads in catch and station data and makes sure only the species necessary are kept
#' ### may want to keep NA (blank in Excel) to select the zero tows
#' removeCAN is a flag if you want tows in Canadian waters removed
#'    need the file called foreign_hauls.csv
#'
#' Necessary column names
#'    SPECIES_CODE
#'    LENGTH
#' @inheritParams pull_catch
#' @param dat data file name
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export

ReadInLengths.fn <- function(dat, verbose = TRUE) {
  lifecycle::deprecate_stop(
    when = "2.8",
    what = "ReadInLengths.fn()",
    details = "This function is no longer used.  Please use pull_bio() to get properly formatted and filtered data."
  )

  return(dat)
}
