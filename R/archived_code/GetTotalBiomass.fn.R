#' Wrapper function  to create the designed based indices 
#'  
#' 
#' @param file the BiomassAbundance csv file
#' @param headerRow header row in the csv file
#' @param fleet fleet number
#' @param season season number
#' @param outputMedian TRUE/FALSE
#'
#' @author Allan Hicks 
#' @export
#' @seealso \code{\link{readInBiomass.fn}}, \code{\link{SS3Biomass.fn}}

GetTotalBiomass.fn <-function(file, headerRow, fleet="EnterFleet", season=1, outputMedian=T) {
    #a wrapper for to output the biomass in SS3 format, reading in from a csv file
    bio <- readInBiomass.fn(file,headerRow)
    SS3Biomass.fn(bio,fleet,season)
}
