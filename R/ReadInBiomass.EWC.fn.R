#' Reads in the triennial survey data and filters the data into what is necessary
#' It reads in catch and station data and makes sure only the species necessary are kept
#' may want to keep NA (blank in Excel) to select the zero tows
#' removeCAN is a flag if you want tows in Canadian waters removed
#' 
#' 
#' Necessary column names
#'    SPECIES_CODE
#'    WEIGHT in kg
#'    DISTANCE_FISHED is in km
#'    NET_WIDTH is in m
#' 
#' @param dir working directory where the dataFile is located
#' @param dat data file name, an R object pre-loaded
#' @param species species name to extract
#' @param removeCAN removes Canadian hauls based on the AFSCforeign_hauls.rda file
#' @param verbose write out comments
#'
#' @author Allan Hicks 
#' @export 

ReadInBiomass.EWC.fn <- function(dir, dat, species=c(NA), removeCAN=TRUE, verbose=TRUE){ 

    totRows <- nrow(dat)
    if("SPECIES_CODE" %in% names(dat)){
      dat <- dat[dat$SPECIES_CODE %in% species,]
      if(verbose) {cat(nrow(dat),"rows kept out of",totRows,"after filtering by species\n")}
    }else{
      if(verbose) {cat("no column in dataFile for 'SPECIES_CODE'. Keeping all columns: n =",nrow(dat),"\n")}
    }
    totRows <- nrow(dat)
    
    if(removeCAN) {
        fpath = system.file("data", "AFSCforeign_hauls.rda", package="nwfscSurvey")
        load(fpath)
        #load("AFSCforeign_hauls.rda")
        foreignHauls = AFSCforeign_hauls
        #foreignHauls <- read.csv(file.path(directory,foreignfile))
        foreignInd <- !(dat$HAULJOIN %in% foreignHauls$HAULJOIN)
        dat <- dat[foreignInd,]
        if(verbose) {cat(sum(foreignInd),"rows kept (or",sum(!foreignInd),"removed) out of",totRows,"after removing foreign hauls\n")}
    }            
    
    #calculate the density (kg/km^2) using net width and distance fished
    dat$areaFished <- dat$DISTANCE_FISHED*(dat$NET_WIDTH/1000)
    tmp <- sum(is.na(dat$areaFished))
    if(tmp>0 | verbose) {cat("There are",tmp,"instances where area swept could not be calculated due to missing values.\n")}
    dat$kgPerKm2 <- dat$WEIGHT/dat$areaFished
    dat$kgPerKm2[is.na(dat$kgPerKm2)&!is.na(dat$areaFished)] <- 0 #the tows with no observation of the species
    return(dat)
}
