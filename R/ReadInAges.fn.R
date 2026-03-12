#' Cleans triennial survey data by year and area
#'
#' @details
#' Reads in the West Coast Triennial survey data and filters the data into what
#' is necessary. It reads in data and makes sure only the species necessary are
#' kept may want to keep NA (blank in Excel) to select the zero tows
#' removeCAN is a flag if you want tows in Canadian waters removed.
#'
#' Necessary column names
#'    SPECIES_CODE
#'    AGE
#'
#' @inheritParams pull_catch
#' @param dat data file name
#' @param subset_years specify the years to retain, default is NULL which will
#' provide 1977, alternative input would be 1980:2002 to remove only 1977.
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export

ReadInAges.fn <- function(dat, subset_years = NULL, verbose = TRUE) {
  lifecycle::deprecate_stop(
    when = "2.8",
    what = "ReadInAges.fn()",
    details = "This function is no longer used.  Please use pull_bio() to get properly formatted and filtered data."
  )

  return(dat)
}
