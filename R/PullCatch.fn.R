#' Pull catch data from the NWFSC data warehouse
#'
#' Pull catch data from the
#' [NWFSC data warehouse](https://www.webapps.nwfsc.noaa.gov/data)
#' for a single species or all observed species, where the latter is specified
#' by leaving both `Name = NULL` and `SciName = NULL`.
#'
#' @details
#' The data available in the warehouse are cleaned pior to being downloaded
#' with the intent that they provide the best available information for use
#' in an index-standardization procedure. The removed samples may be of use
#' to others with a less-restrictive goal than producing an index of abundance.
#' For example, life-stage samples are excluded because they are not collected
#' using the same protocols as standard samples.
#' To download all data, we currently recommend going to the
#' [NWFSC data warehouse](https://www.webapps.nwfsc.noaa.gov/data)
#' and using the csv link to extract data for a single species at a time.
#' In the future, we hope to add functionality to this package such that
#' downloading all data can be done easily within this function.
#' See [Issue #43](https://github.com/pfmc-assessments/nwfscSurvey/issues/43)
#' for more information.
#'
#' @param Name A character entry with the desired common name of the
#' species you want to pull data for from the data warehouse.
#' Use a vector of names if you want information for more than one species or
#' if the desired species is included in the database using more than one name,
#' e.g., vermilion rockfish (see the example below).
#' Use the `SciName` argument if you know the latin name.
#' @param SciName A character entry with the desired scientific name of the
#' species you want to pull data for from the data warehouse.
#' Use a vector of names if you want information for more than one species or
#' if the desired species is included in the database using more than one name,
#' e.g., vermilion rockfish (see the example below).
#' Use the `Name` argument if you know the common name.
#' @param YearRange An integer vector of length two with the
#' range of years to pull data for.
#' @param SurveyName A character entry from one of the following options that
#' specifies which survey to pull the data for:
#'   * Triennial,
#'   * AFSC.Slope,
#'   * NWFSC.Combo,
#'   * NWFSC.Slope,
#'   * NWFSC.Shelf,
#'   * NWFSC.Hypoxia,
#'   * NWFSC.Santa.Barb.Basin,
#'   * NWFSC.Shelf.Rockfish (not yet working),
#'   * NWFSC.Hook.Line (not yet working),
#'   * NWFSC.Video,
#'   * Triennial.Canada
#'
#' Currently, you must pull data one survey at a time, though we are working on
#' allowing for a vector of survey names and
#' `NWFSC.Shelf.Rockfish` and `NWFSC.Hook.Line` are not supported.
#' The default of `NULL` is a placeholder that must be replaced with an entry.
#' @param SaveFile Deprecated with {nwfscSurvey} 2.3. Output will be save automatically
#'   if the Dir input is specified.
#' @param Dir The directory where you want the output file to be saved.
#'   The name of the file within `Dir` will start with Catch_ and end with .rdata.
#'   Default NULL which will not save an output file.
#' @template verbose
#'
#' @author Chantel Wetzel (maintainer) based on code by John Wallace
#' @export
#'
#' @import chron
#' @importFrom stringr str_replace_all
#' @importFrom dplyr left_join rename
#'
#' @examples
#' \dontrun{
#' # SurveyName is only arg that has to be specified
#' dat <- PullCatch.fn(SurveyName = "NWFSC.Combo")
#'
#' # Example with specified common name
#' catch_dat <- PullCatch.fn(
#'   Name = "vermilion rockfish",
#'   SurveyName = "NWFSC.Combo"
#' )
#'
#' # Example with specified scientific name
#' catch_dat <- PullCatch.fn(
#'   SciName = "Eopsetta jordani",
#'   SurveyName = "NWFSC.Combo"
#' )
#'
#' # Example with multiple names
#' catch_dat <- PullBio.fn(Name = c(
#'   "vermilion rockfish",
#'   "vermilion and sunset rockfish"
#' ), SurveyName = "NWFSC.Combo")
#'
# catch_dat <- PullCatch.fn(SciName = c("Sebastes miniatus",
# "Sebastes sp. (crocotulus)","Sebastes sp. (miniatus / crocotulus)"),
# SurveyName = "NWFSC.Combo")
#' }
#'
PullCatch.fn <- function(
    Name = NULL,
    SciName = NULL,
    YearRange = c(1980, 5000),
    SurveyName = NULL,
    SaveFile = lifecycle::deprecated(),
    Dir = NULL,
    verbose = TRUE) {
  lifecycle::deprecate_soft(
    when = "2.3",
    what = "nwfscSurvey::PullCatch.fn()",
    details = "Please switch to pull_catch()."
  )

  if (lifecycle::is_present(SaveFile)) {
    lifecycle::deprecate_warn(
      when = "2.3",
      what = "nwfscSurvey::PullCatch.fn(SaveFile =)"
    )
  }

  Out <- pull_catch(
    common_name = Name,
    sci_name = SciName,
    years = YearRange,
    survey = SurveyName,
    dir = Dir,
    convert = TRUE,
    verbose = verbose
  )

  return(Out)
}
