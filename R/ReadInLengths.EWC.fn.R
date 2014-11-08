###############################################################################################
#Reads in the triennial survey data and filters the data into what is necessary
#It reads in catch and station data and makes sure only the species necessary are kept
#### may want to keep NA (blank in Excel) to select the zero tows
#removeCAN is a flag if you want tows in Canadian waters removed
#   need the file called foreign_hauls.csv
#
#
#Necessary column names
#   SPECIES_CODE
#   LENGTH
###############################################################################
ReadInLengths.EWC.fn <- function(dataFile,directory,species=c(NA),removeCAN=T,verbose=F) {
    dat <- read.csv(paste(directory,dataFile,sep="\\"))
    totRows <- nrow(dat)
    dat <- dat[dat$SPECIES_CODE %in% species,]
    if(verbose) {cat(nrow(dat),"rows kept out of",totRows,"after filtering by species\n")}
    totRows <- nrow(dat)
    
    if(removeCAN) {
        foreignHauls <- read.csv(paste(directory,"foreign_hauls.csv",sep="\\"))
        foreignInd <- !(dat$HAULJOIN %in% foreignHauls$HAULJOIN)
        dat <- dat[foreignInd,]
        if(verbose) {cat(sum(foreignInd),"rows kept (or",sum(!foreignInd),"removed) out of",totRows,"after removing foreign hauls\n")}
        totRows <- nrow(dat)
    }        
    
    dat <- dat[!is.na(dat$LENGTH),]
    if(verbose) {cat(nrow(dat),"rows kept out of",totRows,"after removing missing lengths\n")}
    
    return(dat)   
}
