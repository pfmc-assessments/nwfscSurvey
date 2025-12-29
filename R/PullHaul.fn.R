#' Pull haul data from the NWFSC data warehouse
#' The website is: https://www.webapps.nwfsc.noaa.gov/data
#' This function can be used to pull haul data and associated covariates
#'
#' @param YearRange range of years to pull data. Defaults to all years, 1977 - present.
#' @param SurveyName survey to pull the data for the options are:
#' Triennial, AFSC.Slope, NWFSC.Combo, NWFSC.Slope, NWFSC.Shelf, NWFSC.Hypoxia,
#' NWFSC.Santa.Barb.Basin, NWFSC.Shelf.Rockfish (NWFSC.Hook.Line but both are not working), NWFSC.Video#'
#' @param SaveFile Deprecated with {nwfscSurvey} 2.3. Output will be save automatically
#'   if the Dir input is specified.
#' @param Dir directory where the file should be saved
#' @template verbose
#'
#' @return Returns a data frame of haul characteristics for satisfactory hauls
#' @author Eric Ward
#' @export
#'
#' @import chron
#'
#' @examples
#' \dontrun{
#' haul_dat <- PullHaul.fn(SurveyName = "NWFSC.Combo", YearRange = c(2003, 2007))
#' haul_dat <- PullHaul.fn()
#' }
#'
PullHaul.fn <- function(
  YearRange = c(1980, 5000),
  SurveyName = NULL,
  SaveFile = lifecycle::deprecated(),
  Dir = NULL,
  verbose = TRUE
) {
  lifecycle::deprecate_soft(
    when = "2.3",
    what = "nwfscSurvey::PullHaul.fn()",
    details = "Please switch to pull_haul()."
  )

  if (lifecycle::is_present(SaveFile)) {
    lifecycle::deprecate_warn(
      when = "2.3",
      what = "nwfscSurvey::PullHaul.fn(SaveFile =)"
    )
  }

  Data <- pull_haul(
    years = YearRange,
    survey = SurveyName,
    dir = Dir,
    verbose = verbose
  )

  return(Data)
}
