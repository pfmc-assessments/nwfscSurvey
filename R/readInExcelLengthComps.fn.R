readInExcelLengthComps.fn <-
function(file,sheet="LengthComps",headerRow=7) {
    #Reads in the stratum numbers from the Excel file provided by Beth (LengthComps Sheet)
    #headerRow is the row number of the column where the data start
    #it doesn't read in the column names correctly, so I put in simplified names. Make sure that these match what is in your Excel spreadsheet
    #written by Allan Hicks, 2009
    nombres <- c("SpeciesCode","ScientificName","Species","Year","Project","AreaSetIdentifier","AreaName","DepthStrataSet","MinStratumDepth","MaxStratumDepth","Length","NumF","NumM","NumUnsexed")

    xx <- readDataFromExcel.fn(file,sheet,headerRow)
    names(xx) <- nombres
    cat("\nNOTE: column names have been modified from the Excel Sheet. You may want to verify that they match.\n")
    return(xx)
}
