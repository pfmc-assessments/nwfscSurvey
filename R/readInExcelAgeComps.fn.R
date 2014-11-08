readInExcelAgeComps.fn <-
function(file,sheet="AgeComps",headerRow=7) {
    #Reads in the stratum numbers from the Excel file provided by Beth (AgeComps Sheet)
    #headerRow is the row number of the column where the data start
    #it doesn't read in the column names correctly, so I put in simplified names. Make sure that these match what is in your Excel spreadsheet
    #written by Allan Hicks, 3/21/09
    nombres <- c("SpeciesCode","Species","Year","Project","AreaName","MinStratumDepth","MaxStratumDepth","Length","Age","NumF","NumM","NumUnsexed","LengthedAgeTally","AgeTallyF","AgeTallyM","AgeTallyU")

    xx <- readDataFromExcel.fn(file,sheet,headerRow)
    names(xx) <- nombres
    cat("\nNOTE: column names have been modified from the Excel Sheet. You may want to verify that they match.\n\n")
    return(xx)
}
