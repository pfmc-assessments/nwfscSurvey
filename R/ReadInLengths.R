#' Deprecated function replaced by pull_bio()
#'
#' @inheritParams pull_catch
#' @param dat data file name
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export

ReadInLengths.fn <- function(dat, verbose = TRUE) {
  lifecycle::deprecate_stop(
    when = "2.8.0",
    what = "ReadInLengths.fn()",
    details = "This function is no longer used.  Please use pull_bio() to get properly formatted and filtered data."
  )

  return(dat)
}
