#' Creates a matrix of length or age composition data WITHOUT expantion
#' Written by Chantel Wetzel to work with the data warehouse data formatting,
#' 
#' @param dir directory this is where the output files will be saved
#' @param datA the read in length comps by the PullBio.fn function
#' @param lgthBins length bins
#' @param sex (0 = unsexed, 1 = females, 2 = males, 3 = females then males) sex value for Stock Synthesis
#' @param partition partition for Stock Synthesis
#' @param fleet fleet number
#' @param nSamps effective sample size for Stock Synthesis
#' @param month month the samples were collected
#' @param printfolder folder where the length comps will be saved
#' @param remove999 the output object by the function will have the 999 column combined with the first length bin
#' @param verbose opt to print out message statements
#'
#' @author Chantel Wetzel
#' @export 

UnexpandedAFs.fn <- function(dir = NULL, datA, ageBins = 1, sex = 3,  partition = 0, fleet = "Enter Fleet",  
                             ageErr = "NA", agelow = -1, agehigh = -1, 
                             nSamps = "Enter Samps", month = "Enter Month", printfolder = "forSS", verbose = TRUE)  {

    # Overwrite inputs to use the same code for lengths as ages
    datL = datA
    lgthBins = ageBins  
    datL$Length_cm = datA$Age

    out = UnexpandedLFs.fn(dir = dir, 
                           datL = datL, 
                           lgthBins = lgthBins, 
                           sex = sex,  
                           partition = partition, 
                           fleet = fleet,  
                           nSamps = nSamps, 
                           month = month, 
                           ageErr = ageErr, 
                           agelow = agelow, 
                           agehigh = agelow, 
                           printfolder = "forSS", 
                           verbose = TRUE) 

    return(out)

}