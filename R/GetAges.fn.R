#' Wrapper function that will read in age comp file and create age-at-length comps for SS 
#' This function is used for the NWFSC shelf-slope and NWFSC slope survey
#' 
#' 
#' @param file The csv file with your composition data.
#' @param headerRow Line number indicating the header row of your csv file. (e.g., headerRow = 7)
#' @param lgthBins Either the interval between length bins or the actual length bins (e.g., lgthBins = 11:47)
#' @param ageBins Either the interval between age bins or the actual age bins (e.g, ageBins = 1:40)
#' @param nSamps Vector of effective sample sizes by year for data.  Value will be written in the effN column. 
#' @param fleet Fleet number 
#' @param season Season number
#' @param partition partition as defined by Stock Synthesis
#' @param ageerr Age error value for Stock Synthesis
#' @param raw raw=T/F, input to define whether or not to expand numbers in the csv file (column header "NumF" and "NumM")
#' @param sep Column separator in 'file'
#'
#' @author Allan Hicks 
#' @export
#' @seealso  \code{\link{readInAgeComps.fn}}, \code{\link{SS3AgeAtLen.fn}}


GetAges.fn <-function(file, headerRow, lgthBins=1, ageBins=1, nSamps="EnterNsamps", fleet="EnterFleet",
		 			  season=1, partition=0, ageerr="EnterAgeErr", raw=T, sep=",") {

    age <- readInAgeComps.fn(file, headerRow=headerRow, sep=sep)
    SS3AgeAtLen.fn(age, lgthBins=lgthBins, ageBins=ageBins, fleet=fleet, season=season, partition=partition, ageerr=ageerr, raw=raw)
}
