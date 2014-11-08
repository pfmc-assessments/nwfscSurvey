readInExcelBiomass.fn <-
function(file,sheet="BiomassAbundance",headerRow=6,colNames=NA) {
    #Reads in the stratum biomasses from the Excel file provided by Beth (BiomassAbundance Sheet)
    #headerRow is the row number of the column where the data start
    #it doesn't read in the column names correctly, so I put in simplified names. Make sure that these match what is in your Excel spreadsheet
    #written by Allan Hicks, 2009

    xx <- readDataFromExcel.fn(file,sheet,headerRow)
    if(is.null(colNames[1])) {
        nombres <- c("Species","ScientificName","SpeciesCode","Year","Project","StrataAreaVersion","AreaSetId","AreaName","SouthernLatitude","NorthernLatitude","DepthStrataSet","MinStratumDepth","MaxStratumDepth","StratumArea","Biomass","Abundance","CpueWeightVar","CpueCountVar","BiomassVar","CV","N","Nbio","Npos","NbioPos")
        names(xx) <- nombres
        cat("NOTE: column names have been modified from the Excel Sheet. You may want to verify that they match.\n")
    }
    if(!is.na(colNames[1])) {
        names(xx) <- colNames
        return(xx)
    }
    return(xx)
}
