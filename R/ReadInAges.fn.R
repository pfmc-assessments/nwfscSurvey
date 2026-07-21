#' Deprecated function replaced by pull_bio()
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
    when = "2.8.0",
    what = "ReadInAges.fn()",
    details = "This function is no longer used.  Please use pull_bio() to get properly formatted and filtered data."
  )

  return(dat)
}
