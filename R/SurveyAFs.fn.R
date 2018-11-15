#' Expands the ages up to the total stratum area then sums over strata
#' Original Version Written by Allan Hicks 16 March 2009
#' Modified by Chantel Wetzel to work with the data warehouse data formatting,
#' add additional options of when to apply the sex ratio, and correct some treatment of unsexed fish
#' weighted by sample size and area
#' NOTE: The age bin called F0 or M0 is retained to show proportion of ages smaller than smallest bin
#' You will want to likely add this to your first age bin and delete this before putting in SS, or
#' start the ageBins argument at the 2nd age bin and F0 will be all fish smaller (hence the first age bin)
#' 
#' @param dir directory this is where the output files will be saved
#' @param datA the biological data frame exctrated from the data warehouse using the PullBio.fn
#' @param datTows the catch data frame extracted from the data warehouse using the PullCatch.fn
#' @param strat.vars the variables used define the stratas. Defaul is bottom depth and latitudes.
#' @param strat.df the created strata matrix with the calculated areas by the createStrataDF.fn function
#' @param femaleMale numbering for female and male fish in the data file. This is opposite to what is used in SS.
#' @param lageBins length bins
#' @param SSout if True the output is in a format pastable into SS dat file
#' @param meanRatioMethod TRUE/FALSE
#' @param gender gender value for Stock Synthesis
#' @param NAs2zero change NAs to zeros
#' @param sexRatioUnsexed sex ratio to apply to any length bins of a certain size or smaller as defined by the maxSizeUnsexed
#' @param maxSizeUnsexed all sizes below this threshold will assign unsexed fish by sexRatio set equal to 0.50, fish larger than this size will have unsexed fish assigned by the calculated sex ratio in the data.
#' @param sexRatioStage 1/2 apply the sex ratio based on the tows (1) or the expanded numbers (2)
#' @param partition partition for Stock Synthesis
#' @param fleet fleet number
#' @param agelow value for SS -1
#' @param agehigh value for SS -1
#' @param ageErr age error vector to apply
#' @param nSamps effective sample size for Stock Synthesis
#' @param month month when the samples were collected
#' @param printfolder folder where the length comps will be saved
#' @param remove999 the output object by the function will have the 999 column combined with the first length bin
#' @param outputStage1 return the first stage expanded data without compiling it for SS
#' @param verbose opt to print out message statements
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' @seealso \code{\link{SurveyLFs.fn}} 


SurveyAFs.fn <- function(dir = NULL, datA, datTows, strat.vars=c("Depth_m","Latitude_dd"), strat.df=NULL, ageBins=1, SSout=TRUE, meanRatioMethod=TRUE,
                             gender=3, NAs2zero=T, sexRatioUnsexed=NA, maxSizeUnsexed=NA, sexRatioStage = 1, partition=0, fleet="Enter Fleet", agelow = "Enter",
                             agehigh = "Enter", ageErr = "Enter", nSamps="Enter Samps", month="Enter Month", printfolder = "forSS",
                             remove999 = TRUE, outputStage1 = FALSE, verbose = TRUE)  {

    # Overwrite inputs to use the same code for lengths as ages
    datL = datA
    lgthBins = ageBins  
    datL$Length_cm = datA$Age

    out = SurveyLFs.fn(dir = dir, datL = datL, datTows = datTows, strat.vars = strat.vars, strat.df = strat.df, 
                       lgthBins = lgthBins, SSout = SSout, meanRatioMethod = meanRatioMethod,
                       gender = gender, NAs2zero= NAs2zero,  sexRatioUnsexed = sexRatioUnsexed, maxSizeUnsexed = maxSizeUnsexed, 
                       sexRatioStage = sexRatioStage, partition = partition,  fleet = fleet, nSamps = nSamps, 
                       agelow = agelow, agehigh = agehigh, ageErr = ageErr, 
                       month = month, printfolder = printfolder, remove999 = remove999,  outputStage1 = outputStage1,
                       verbose = verbose)
    return(out)
}
