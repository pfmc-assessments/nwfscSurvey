#' Expands the ages up to the total stratum area then sums over strata
#' Original Version Written by Allan Hicks 16 March 2009
#' Modified by Chantel Wetzel to work with the data warehouse data formatting,
#' add additional options of when to apply the sex ratio, and correct some treatment of unsexed fish
#' weighted by sample size and area
#' NOTE: The age bin called F0 or M0 is retained to show proportion of ages smaller than smallest bin
#' You will want to likely add this to your first age bin and delete this before putting in SS, or
#' start the ageBins argument at the 2nd age bin and F0 will be all fish smaller (hence the first age bin)
#'
#' @template dir 
#' @template bds 
#' @template catch 
#' @param datA *Deprecated* Replaced by bds, the biological data frame exctrated from the data warehouse using the PullBio.fn
#' @param datTows *Deprecated* Replaced by catch, the catch data frame extracted from the data warehouse using the PullCatch.fn
#' @param strat.vars The variables used define the stratas. Default is bottom depth and latitudes: c("Depth_m", "Latitude_dd").
#' @param strat.df The created strata matrix with the calculated areas by the createStrataDF.fn function.
#' @param ageBins User defined age bins to create composition data for. If not specified (default value of 1) the
#' range of ages to create composition data will be based on range of observerved ages.
#' @param SSout Default is TRUE. If set to TRUE the output files are formatted for use in Stock Synthesis.
#' @template meanRatioMethod 
#' @template sex 
#' @param NAs2zero *Deprecated* Now will always replace NAs with zeros
#' @template sexRatioUnsexed 
#' @param maxAgeUnsexed All ages below this threshold will assign unsexed fish by the sexRatioUnsexed value, fish older than this age will have unsexed fish assigned by the calculated sex ratio in the data.
#' @param sexRatioStage 1/2 apply the sex ratio based on the tows (1) or the expanded numbers (2)
#' @template partition 
#' @template fleet 
#' @param agelow *Deprecated* A default value of -1 will be printed in the marginal age composition files
#' @param agehigh *Deprecated* A default value of -1 will be printed in the marginal age composition files
#' @param ageErr Default NA. Value of age error vector to use within Stock Synthesis for the data. This input is only 
#' used when creating age compostion data in the \code{\link{SurveyAFs.fn}} or \code{\link{SurveyAgeAtLen.fn})
#' @tempalte nSamps 
#' @template month 
#' @template printfolder 
#' @param remove999 *Deprecated* The -999 column is no longer provided
#' @param outputStage1 Return the first stage expansion - only expanded up to the tow level
#' @template verbose 
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' @seealso \code{\link{SurveyLFs.fn}}
#'
#'
SurveyAFs.fn <- function(dir = NULL, bds, catch, datA = lifecycle::deprecated(), datTows =  lifecycle::deprecated(), strat.vars = c("Depth_m", "Latitude_dd"), strat.df = NULL, ageBins = 1, SSout = TRUE, meanRatioMethod = TRUE,
                         sex = 3, NAs2zero =  lifecycle::deprecated(), sexRatioUnsexed = NA, maxAgeUnsexed = NA, sexRatioStage = 1, partition = 0, fleet = "Enter Fleet", agelow = lifecycle::deprecated(),
                         agehigh = lifecycle::deprecated(), ageErr = "Enter", nSamps = "Enter Samps", month = "Enter Month", printfolder = "forSS",
                         remove999 =  lifecycle::deprecated(), outputStage1 = FALSE, verbose = TRUE) {

    if(missing(strat.df)){
    stop("Must specify the strata via the strat.df function input.")
  }

  if(missing(bds) | missing(catch)){
    stop("Must specify the both the bds and catch in the function input.")
  }

  if (lifecycle::is_present(NAs2zero)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(NAs2zero = )"),
      details = paste0(
        "No longer used. All NAs are converted to 0s."
        )
      )
  }

  if (lifecycle::is_present(datA)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(datA = )"),
      details = paste0(
        "No longer used. Biological data is now passed via the bds function input."
        )
      )
  }

  if (lifecycle::is_present(datTows)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(datTows = )"),
      details = paste0(
        "No longer used. Catch data is now passed via the catch function input."
        )
      )
  }

  if (lifecycle::is_present(remove999)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(remove999 = )"),
      details = paste0(
        "This column is now always removed."
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

  out <- SurveyLFs.fn(
    dir = dir, bds = bds, catch = catch, strat.vars = strat.vars, strat.df = strat.df,
    lgthBins = lgthBins, SSout = SSout, meanRatioMethod = meanRatioMethod,
    sex = sex, sexRatioUnsexed = sexRatioUnsexed, maxSizeUnsexed = maxAgeUnsexed,
    sexRatioStage = sexRatioStage, partition = partition, fleet = fleet, nSamps = nSamps,
    ageErr = ageErr,month = month, printfolder = printfolder, outputStage1 = outputStage1,
    verbose = verbose
  )
  return(out)
}
