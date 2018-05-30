#' Wrapper function that will read in age comp file and create age-at-length comps for SS from an excel file
#' This function is used for the NWFSC shelf-slope and NWFSC slope survey
#' Have not tested this function for reading from excel file
#' 
#' 
#' @param file The excel file with your composition data.
#' @param sheet Name of sheet to be read in the excel file
#' @param headerRow Line number indicating the header row of your csv file. (e.g., headerRow = 7)
#' @param lgthBins Vector of length bins (e.g., lgthBins = 11:47)
#' @param ageBins Vector of age bins (e.g, ageBins = 1:40)
#' @param nSamps Vector of effective sample sizes by year for data.  Value will be written in the effN column. 
#' @param fleet Fleet number 
#' @param season Season number
#' @param partition Partition value for composition data
#' @param ageerr Age error value for SS
#' @param raw raw=T/F, input to define whether or not to expand numbers in the csv file (column header "NumF" and "NumM")
#'
#' @author Allan Hicks 
#' @export
#' @seealso  \code{\link{readInExcelAgeComps.fn}}, \code{\link{SS3AgeAtLen.fn}}

GetAgesExcel.fn <-function(file, sheet="AgeComps", headerRow, lgthBins=1, ageBins=1, nSamps="EnterNsamps",
					       fleet="EnterFleet", season=1, partition=0, ageerr="EnterAgeErr", raw=T) {
    age <- readInExcelAgeComps.fn(file,sheet=sheet,headerRow=headerRow)
    SS3AgeAtLen.fn(age,lgthBins=lgthBins,ageBins=ageBins,fleet=fleet,season=season,partition=partition,ageerr=ageerr,raw=raw)
}
