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
#' @param survey name of the survey data to be extracted. Options are "Tri.Shelf" and "AFSC.Slope".
#' @param species species name to extract
#' @param removeCAN removes Canadian hauls based on the AFSCforeign_hauls.rda file
#' @param verbose write out comments
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export 

ReadInBiomass.EWC.fn <- function(dir, dat, survey, species=c(NA), removeCAN=TRUE, subset_years = NULL, verbose=TRUE){ 

    # Filter for the survey
    ind = dat$SURVEY == survey
    dat = dat[ind,]
    if (dim(dat)[1] == 0) { stop(cat("The survey name did not match either Tri.Shelf or AFSC.Slope.\n"))}
    cat("Catches from only the", survey, "have been retained.\n")

    if(length(subset_years)>0) {
        all.yrs = unique(dat$YEAR)
        ind = which(all.yrs%in%subset_years)
        dat = dat[dat$YEAR%in%subset_years,] 
        cat("Catches from", all.yrs[-ind], "have been removed.\n")}

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

    # Add specific names for columns to be used later
    dat$catchPerArea <- dat$kgPerKm2
    dat$year <- dat$YEAR
    return(dat)
}
