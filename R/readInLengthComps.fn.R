readInLengthComps.fn <-
function(file,headerRow=7,sep=",",
                                    colNames=c("SpeciesCode","ScientificName","Species","Year","Project","AreaSetIdentifier","AreaName","DepthStrataSet","MinStratumDepth","MaxStratumDepth","Length","NumF","NumM","NumUnsexed")
) {
    #Reads in the stratum numbers from a csv file (usually converted from Excel file provided by Beth (LengthComps Sheet))
    #headerRow is the row number of the column where the data start
    #it doesn't read in the column names correctly, so I put in simplified names. Make sure that these match what is in your Excel spreadsheet
    #written by Allan Hicks, 2011

    xx <- read.table(file,skip=headerRow-1,sep=sep,header=T)
    if(length(colNames) == ncol(xx)) {
        names(xx) <- colNames
        cat("NOTE: column names have been modified from the csv file. You may want to verify that they match.\n")
    }else{
      cat("NOTE: column names have not been changed because 'colNames' input does not match number of columns.\n",
          "     some functions might not work as a result.\n",
          "     length(colNames):",length(colNames),"   columns in input file:",ncol(xx),"\n",
          "     First row of input file:\n")
      print(head(xx,1))
    }
    return(xx)
}
