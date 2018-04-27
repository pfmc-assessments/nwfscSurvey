#' Wrapper function  to create the designed based indices 
#'  
#' @param dir directory of the excel file
#' @param file the excel file
#' @param sheet sheet name
#' @param printfolder the folder where files will be saved
#' @param headerRow header row in the csv file
#' @param fleet fleet number
#' @param season season number
#' @param outputMedian TRUE/FALSE
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' \code{\link{readInExcelBiomass.fn}}, \code{\link{SS3Biomass.fn}}

GetTotalBiomassExcel.fn <-
function(dir, file, sheet="BiomassAbundance", printfolder = "forSS", headerRow, fleet="EnterFleet", season=1, outputMedian=T) {
    #a wrapper for to output the biomass in SS3 format, reading directly from Excel
    bio <- readInExcelBiomass.fn(file, sheet, headerRow)
    out <- SS3Biomass.fn(bio,fleet,season)
    # save output as a csv
    plotdir <- file.path(dir,printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if(is.na(plotdir.isdir) | !plotdir.isdir){
      dir.create(plotdir)
    }
    write.csv(out, file = paste0(plotdir,"/NWFSC_BTS_design_based_indices.csv"))
    return(out)
}
