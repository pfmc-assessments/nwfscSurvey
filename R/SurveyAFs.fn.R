#' Create marginal age composition data
#'
#' @details
#' Create expanded marginal age composition data based on the pre-specified strata.
#' This function is designed to be used with catch pulled using [pull_catch()] and
#' biological data pulled using [pull_bio()]. The default output is formatted based
#' on the formatting required by Stock Synthesis.
#'
#' @template dir
#' @param datA the biological data frame exctrated from the data warehouse using the [pull_bio()]
#' @param datTows the catch data frame extracted from the data warehouse using the [pull_catch()]
#' @template strat.vars
#' @template strat.df
#' @param ageBins Vector of age bins to create age compositions across. Values above or below the
#'   minimum or maximum values, respectively, are grouped into the first age or plus group age.
#' @param SSout A logical with the default of `TRUE`. If `TRUE`, the output
#'   is returned in a format that can be directly pasted into an SS3 data file.
#' @param meanRatioMethod A logical with the default of `TRUE`. If `TRUE`, then
#'   the mean ratio is implemented instead of the total ratio. Search the
#'   source code for the equations if more information is needed.
#' @template sex
#' @param NAs2zero A logical specifying if `NA`s should be changed to zeros.
#'   The default is `TRUE`.
#' @param sexRatioUnsexed sex ratio to apply to any length bins of a certain size or smaller as defined
#'   by the maxSizeUnsexed
#' @param maxSizeUnsexed all sizes below this threshold will assign unsexed fish by sexRatio set equal to 0.50, fish larger than this size will have unsexed fish assigned by the calculated sex ratio in the data.
#' @param sexRatioStage 1/2 apply the sex ratio based on the tows (1) or the expanded numbers (2)
#' @template partition
#' @template agelow
#' @template agehigh
#' @template ageErr
#' @param nSamps Vector of integer sample sizes. A vector of sample sizes for
#' all years in `datA` is required if a vector is provided. The input vector will be included in the
#' output marginal age composition data.  One option for calculating input sample size is the [GetN.fn()].
#' The default is "Enter Samps".
#' @template month
#' @template printfolder
#' @param remove999 The output object by the function will have the 999 column combined with the first age bin.
#'   Default TRUE.
#' @param outputStage1 return the first stage expanded data without compiling it for SS
#' @template verbose
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' @seealso \code{\link{SurveyLFs.fn}}


SurveyAFs.fn <- function(
  dir = NULL,
  datA,
  datTows,
  strat.vars = c("Depth_m", "Latitude_dd"),
  strat.df = NULL,
  ageBins = 1,
  SSout = TRUE,
  meanRatioMethod = TRUE,
  sex = 3,
  NAs2zero = T,
  sexRatioUnsexed = NA,
  maxSizeUnsexed = NA,
  sexRatioStage = 1,
  partition = 0,
  fleet = "Enter Fleet",
  agelow = "Enter",
  agehigh = "Enter",
  ageErr = "Enter",
  nSamps = "Enter Samps",
  month = "Enter Month",
  printfolder = "forSS3",
  remove999 = TRUE,
  outputStage1 = FALSE,
  verbose = TRUE) {

  # Overwrite inputs to use the same code for lengths as ages
  datL <- datA
  lgthBins <- ageBins
  datL$Length_cm <- datA$Age

  out <- SurveyLFs.fn(
    dir = dir,
    datL = datL,
    datTows = datTows,
    strat.vars = strat.vars,
    strat.df = strat.df,
    lgthBins = lgthBins,
    SSout = SSout,
    meanRatioMethod = meanRatioMethod,
    sex = sex,
    NAs2zero = NAs2zero,
    sexRatioUnsexed = sexRatioUnsexed,
    maxSizeUnsexed = maxSizeUnsexed,
    sexRatioStage = sexRatioStage,
    partition = partition,
    fleet = fleet,
    nSamps = nSamps,
    agelow = agelow,
    agehigh = agehigh,
    ageErr = ageErr,
    month = month,
    printfolder = printfolder,
    remove999 = remove999,
    outputStage1 = outputStage1,
    verbose = verbose
  )
  return(out)
}
