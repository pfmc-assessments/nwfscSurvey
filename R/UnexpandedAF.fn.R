#' Creates a matrix of length or age composition data WITHOUT expantion
#' Written by Chantel Wetzel to work with the data warehouse data formatting,
#'
#' @template dir 
#' @param datA *Deprecated* Replaced by bds
#' @param ageBins User defined age bins to create composition data for.
#' @templat sex 
#' @template partition 
#' @template fleet 
#' @param agelow *Deprecated* A default value of -1 will be printed in the marginal age composition files
#' @param agehigh *Deprecated* A default value of -1 will be printed in the marginal age composition files
#' @param ageErr Default NA. Value of age error vector to use within Stock Synthesis for the data. This input is only 
#' used when creating age compostion data in the \code{\link{SurveyAFs.fn}}
#' @template month 
#' @template printfolder
#' @template verbose
#'
#' @author Chantel Wetzel
#' @export

UnexpandedAFs.fn <- function(dir = NULL, bds, datA = lifecycle::deprecated(), ageBins = 1, sex = 3, partition = 0, fleet = "Enter Fleet",
                             ageErr = "NA", agelow = lifecycle::deprecated(), agehigh = lifecycle::deprecated(), month = "Enter Month", 
                             printfolder = "forSS", verbose = TRUE) {

  if (lifecycle::is_present(datA)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(datA = )"),
      details = paste0(
        "No longer used. Biological data is now passed via the bds function input."
        )
      )
  }

  if (lifecycle::is_present(agelow)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(agelow = )"),
      details = paste0(
        "This column is now always removed."
        )
      )
  }

  if (lifecycle::is_present(agehigh)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(agehigh = )"),
      details = paste0(
        "This column is now always removed."
        )
      )
  }

  # Overwrite inputs to use the same code for lengths as ages
  lgthBins <- ageBins
  bds$Length_cm <- bds$Age

  out <- UnexpandedLFs.fn(
    dir = dir,
    bds = bds,
    lgthBins = lgthBins,
    sex = sex,
    partition = partition,
    fleet = fleet,
    month = month,
    ageErr = ageErr,
    printfolder = printfolder,
    verbose = verbose
  )

  return(out)
}
