#' Wrapper function  to create the designed based indices 
#'  
#' @param file the excel file
#' @param sheet sheet name
#' @param headerRow header row in the csv file
#' @param fleet fleet number
#' @param season season number
#' @param outputMedian TRUE/FALSE
#'
#' @author Allan Hicks 
#' @export
#' \code{\link{readInExcelBiomass.fn}}, \code{\link{SS3Biomass.fn}}

GetTotalBiomassExcel.fn <-
function(file, sheet="BiomassAbundance", headerRow, fleet="EnterFleet", season=1, outputMedian=T) {
    #a wrapper for to output the biomass in SS3 format, reading directly from Excel
    bio <- readInExcelBiomass.fn(file, sheet, headerRow)
    out <- SS3Biomass.fn(bio,fleet,season)
    # save output as a csv
    dir.create(paste0(getwd(),"/forSS"), showWarnings = FALSE)
    write.csv(out, file = "/forSS/NWFSC_BTS_design_based_indices.csv")
    return(out)
}
