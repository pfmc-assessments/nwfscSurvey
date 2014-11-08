readInBiomass.fn <-
function(filename,headerRow=1,sep=",",
                                colNames = c("Species","ScientificName","SpeciesCode","Year","Project","StrataAreaVersion","AreaSetId","AreaName","SouthernLatitude","NorthernLatitude","DepthStrataSet","MinStratumDepth","MaxStratumDepth","StratumArea","Biomass","Abundance","CpueWeightVar","CpueCountVar","BiomassVar","CV","N","Nbio","Npos","NbioPos")                    )
{
    #Reads in the stratum biomasses from the text file saved from the Excel worksheet
    #headerRow is the line number where the data start (use to skip over header lines)
    #you can keep your column names or use the simplified names I provide. Make sure that these match what is in your Excel spreadsheet or make sense!
    #written by Allan Hicks, 2011

    xx <- read.table(filename,skip=headerRow-1,sep=sep,header=T)
    if(length(colNames) == ncol(xx)) {
        names(xx) <- colNames
        cat("NOTE: column names have been modified from the csv file. You may want to verify that they match.\n")
    }
    return(xx)
}
