#' Pull biological data (age, length, weight) from the NWFSC data warehouse
#' The website is: https://www.webapps.nwfsc.noaa.gov/data
#' This function can be used to pull a single species or all observed species
#' In order to pull all species leave Name = NULL and SciName = NULL
#'
#' @param Name  common name of species data to pull from the data warehouse
#' @param SciName scientific name of species data to pull from the data warehouse
#' @param YearRange range of years to pull data
#' @param SurveyName survey to pull the data for the options are:
#' Triennial, AFSC.Slope, NWFSC.Combo, NWFSC.Slope, NWFSC.Shelf, NWFSC.Hypoxia,
#' NWFSC.Santa.Barb.Basin, NWFSC.Shelf.Rockfish (NWFSC.Hook.Line but both are not working), NWFSC.Video#'
#' @param SaveFile Deprecated with {nwfscSurvey} 2.3. Output will be save automatically
#'   if the Dir input is specified.
#' @param Dir The directory where you want the output file to be saved.
#'   The name of the file within `Dir` will start with Catch_ and end with .rdata.
#'   Default NULL which will not save an output file.
#' @template verbose
#'
#' @author Chantel Wetzel based on code by John Wallace
#' @export
#'
#' @import chron
#' @importFrom dplyr rename
#' @importFrom stringr str_replace_all
#'
#' @examples
#' \dontrun{
#' # SurveyName is only arg that has to be specified
#' bio_dat <- PullBio.fn(SurveyName = "NWFSC.Combo")
#'
#' # Example with specified common name
#' bio_dat <- PullBio.fn(
#'   Name = "vermilion rockfish",
#'   SurveyName = "NWFSC.Combo"
#' )
#'
#' # Example with specified scientific name
#' bio_dat <- PullBio.fn(
#'   SciName = "Eopsetta jordani",
#'   SurveyName = "NWFSC.Combo"
#' )
#'
#' # Example with multiple names
#' bio_dat <- PullBio.fn(
#'   SciName = c("Sebastes aurora", "Eopsetta jordani"),
#'   SurveyName = "NWFSC.Combo"
#' )
#   bio_dat <- PullBio.fn(Name = c("Sunset rockfish", "vermilion rockfish",
#     "vermilion and sunset rockfish"), SurveyName = "NWFSC.Combo")
#' }
#'
PullBio.fn <- function(
    Name = NULL,
    SciName = NULL,
    YearRange = c(1980, 5000),
    SurveyName = NULL,
    SaveFile = lifecycle::deprecated(),
    Dir = NULL,
    verbose = TRUE) {
  lifecycle::deprecate_soft(
    when = "2.3",
    what = "nwfscSurvey::PullBio.fn()",
    details = "Please switch to pull_bio()."
  )

  if (lifecycle::is_present(SaveFile)) {
    lifecycle::deprecate_warn(
      when = "2.3",
      what = "nwfscSurvey::PullBio.fn(SaveFile =)"
    )
  }

  Data <- pull_bio(
    common_name = Name,
    sci_name = SciName,
    years = YearRange,
    survey = SurveyName,
    dir = Dir,
    convert = TRUE,
    verbose = TRUE
  )

  return(Data)
}
