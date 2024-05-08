#' Creates a matrix of unexpanded age compositions
#'
#' @template dir
#' @param datA A data frame of length-composition data returned from
#'   [pull_bio()].
#' @param ageBins Vector of age bins to create age compositions across. Values above or below the
#'   minimum or maximum values, respectively, are grouped into the first age or plus group age.
#' @template ageErr
#' @template agelow
#' @template agehigh
#' @template sex
#' @template partition
#' @template fleet
#' @template month
#' @template printfolder
#' @template verbose
#'
#' @author Chantel Wetzel
#' @export

UnexpandedAFs.fn <- function(
  dir = NULL,
  datA,
  ageBins = 1,
  sex = 3,
  partition = 0,
  fleet = "Enter Fleet",
  ageErr = "NA",
  agelow = -1,
  agehigh = -1,
  month = "Enter Month",
  printfolder = "forSS3",
  verbose = TRUE) {

  # Overwrite inputs to use the same code for lengths as ages
  datL <- datA
  lgthBins <- ageBins
  datL$Length_cm <- datA$Age

  out <- UnexpandedLFs.fn(
    dir = dir,
    datL = datL,
    lgthBins = lgthBins,
    sex = sex,
    partition = partition,
    fleet = fleet,
    month = month,
    ageErr = ageErr,
    agelow = agelow,
    agehigh = agelow,
    printfolder = "forSS",
    verbose = TRUE
  )

  return(out)
}
