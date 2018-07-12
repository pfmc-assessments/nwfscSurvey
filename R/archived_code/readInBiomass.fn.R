#' Reads in the stratum biomasses from the text file saved from the Excel worksheet
#' headerRow is the line number where the data start (use to skip over header lines)
#' you can keep your column names or use the simplified names I provide.
#' Make sure that these match what is in your Excel spreadsheet or make sense!
#' 
#' @param filename csv file
#' @param headerRow line of header row in excel file
#' @param sep seperator
#' @param colNames column names the overwrite the one in the csv file
#'
#' @author Allan Hicks 
#' @export 
readInBiomass.fn <-function(filename,headerRow=1,sep=",", 
                            colNames = c("Species","ScientificName","SpeciesCode","Year","Project",
                            "StrataAreaVersion","AreaSetId","AreaName","SouthernLatitude","NorthernLatitude",
                            "DepthStrataSet","MinStratumDepth","MaxStratumDepth","StratumArea","Biomass",
                            "Abundance","CpueWeightVar","CpueCountVar","BiomassVar","CV","N","Nbio","Npos","NbioPos")) {

    xx <- read.table(filename,skip=headerRow-1,sep=sep,header=T)
    if(length(colNames) == ncol(xx)) {
        names(xx) <- colNames
        cat("NOTE: column names have been modified from the csv file. You may want to verify that they match.\n")
    }
    return(xx)
}
