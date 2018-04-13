#' Reads in the West Coast Trienial survey data and filters the data into what is necessary
#' It reads in data and makes sure only the species necessary are kept
#' may want to keep NA (blank in Excel) to select the zero tows
#' removeCAN is a flag if you want tows in Canadian waters removed
#' 
#' Necessary column names
#'    SPECIES_CODE
#'    AGE
#' 
#' @param dataFile data file name
#' @param directory working directory where the dataFile is located
#' @param species species name to extract
#' @param removeCAN removes Canadian hauls based on the AFSCforeign_hauls.rda file
#' @param verbose write out comments
#'
#' @author Allan Hicks 
#' @export 

ReadInAges.EWC.fn <- function(dataFile, directory, species=c(NA), removeCAN=T, verbose=F) {
    dat <- read.csv(paste(directory, dataFile,sep="\\"))
    totRows <- nrow(dat)
    dat <- dat[dat$SPECIES_CODE %in% species,]
    if(verbose) {cat(nrow(dat),"rows kept out of",totRows,"after filtering by species\n")}
    totRows <- nrow(dat)
    
    if(removeCAN) {
        load("AFSCforeign_hauls.rda")
        foreignHauls = AFSCforeign_hauls
        #foreignHauls <- read.csv(paste(directory,"foreign_hauls.csv",sep="\\"))
        foreignInd <- !(dat$HAULJOIN %in% foreignHauls$HAULJOIN)
        dat <- dat[foreignInd,]
        if(verbose) {cat(sum(foreignInd),"rows kept (or",sum(!foreignInd),"removed) out of",totRows,"after removing foreign hauls\n")}
        totRows <- nrow(dat)
    }        
    
    dat <- dat[!is.na(dat$AGE),]
    if(verbose) {cat(nrow(dat),"rows kept out of",totRows,"after removing missing ages\n")}
    
    return(dat)   
}
