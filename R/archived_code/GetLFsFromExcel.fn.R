#' Get length frequencies from an Excel file
#'
#' Wrapper function that calls on readInLengthComps.fn to get length frequencies
#' from NWFSC trawl survey data packages and then passes them to the SS3LF.fn for
#' formatting as required by the Stock Synthesis data file
#'
#' @param file      an excel file containing length comp information
#' @param sheet     name of sheet to be read in the excel file
#' @param headerRow the row number of the column where the data start
#' @param lgthBins  either the interval between length bins or the actual length bins
#' @param gender    gender code as used by Stock Synthesis
#' @param nSamps    value or placeholder for sample size column
#' @param fleet     value or placeholder for fleet number column
#' @param season    season =1
#' @param partition partition as defined by Stock Synthesis
#' @param NAs2zero  determines if NA values will be changed to 0.0
#' @param sep       column separator in 'file'
#' @param sexRatioUnsexed proportion used to assign unsexed fish to females
#' \itemize{
#' \item if a single number, it is only used for sizes at or below
#' maxSizeUnsexed, and sex ratio for size bins above that are calculated
#' using the number of males and females observed in the same size class
#' (or one lower if none available in the same bin)
#'
#' \item if a vector (maybe to be implemented), it must be the same length
#' as the lgthBins and indicates the proportion of unsexed assigned to females
#' for each length bin
#'
#' \item if it is NA, unsexed fish are omitted WHEN GENDER=3 (THIS IS THE DEFAULT)
#' }
#' @param maxSizeUnsexed determines the maximum size at which the sexRatioUnsexed
#' is applied. If sexRatioUnsexed is a vector, this is ignored.
#' @author Allan C. Hicks, Felipe Hurtado-Ferro, Ian G. Taylor
#' @export
#' @seealso \code{\link{readInExcelLengthComps.fn}}, \code{\link{SS3LF.fn}}


GetLFsFromExcel.fn <-function(file, sheet="LengthComps", headerRow, lgthBins=1, gender=3, nSamps="EnterNsamps", fleet="EnterFleet",
							  season=1, partition=0, NAs2zero=T, sexRatioUnsexed=NA, maxSizeUnsexed=NA) {
    len <- readInExcelLengthComps.fn(file,sheet=sheet,headerRow=headerRow)
    SS3LF.fn(len,lgthBins=lgthBins,gender=gender,nSamps=nSamps,fleet=fleet,season=season,partition=partition,NAs2zero,sexRatioUnsexed=NA,maxSizeUnsexed=NA)
}
