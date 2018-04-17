#' Reads in the stratum biomasses from the Excel file provided by Beth (BiomassAbundance Sheet)
#' headerRow is the row number of the column where the data start
#' 
#' 
#' @param file excel file name
#' @param sheet sheet name in excel file
#' @param headerRow line of header row in excel file
#'
#' @author Allan Hicks & Chantel Wetzel
#' @export 
#' \code{\link{readDataFromExcel.fn}}


readInExcelBiomass.fn <-function(file, sheet="BiomassAbundance", headerRow=6) { #, colNames=NA) {

    xx <- readDataFromExcel.fn(file,sheet,headerRow)
    #if(is.null(colNames[1])) {
    #    nombres <- c("Species","ScientificName","SpeciesCode","Year","Project","StrataAreaVersion",
    #                 "AreaSetId","AreaName","SouthernLatitude","NorthernLatitude","DepthStrataSet",
    #                 "MinStratumDepth","MaxStratumDepth","StratumArea","Biomass","Abundance",
    #                 "CpueWeightVar","CpueCountVar","BiomassVar","CV","N","Nbio","Npos","NbioPos")
    #    names(xx) <- nombres
    #    cat("NOTE: column names have been modified from the Excel Sheet. You may want to verify that they match.\n")
    #}
    #if(!is.na(colNames[1])) {
    #    names(xx) <- colNames
    #    return(xx)
    #}
    return(xx)
}
