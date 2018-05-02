#' Reads in the triennial survey data and filters the data into what is necessary
#' It reads in catch and station data and makes sure only the species necessary are kept
#' ### may want to keep NA (blank in Excel) to select the zero tows
#' removeCAN is a flag if you want tows in Canadian waters removed
#'    need the file called foreign_hauls.csv
#' 
#' Necessary column names
#'    SPECIES_CODE
#'    LENGTH
#' 
#' @param dir working directory where the datis located - not currently used
#' @param data data file name
#' @param species species name
#' @param survey name of survey data to use. Options are "Tri.Shelf" or "AFSC.Slope"
#' @param subset_years specify the years to retain, default is NULL which will provide 1977, alternative input would be 1980:2002 to remove only 1977.
#' @param removeCAN switch to remove Canadian hauls
#' @param verbose print out comments
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export 

ReadInLengths.EWC.fn <- function(dir, dat, species=c(NA), survey, subset_years = NULL, removeCAN=TRUE, verbose=TRUE) {

    dat = dat$Lengths[dat$Lengths$SURVEY==survey,]

    if(length(subset_years)>0) { dat = dat[dat$YEAR%in%subset_years,] }
    totRows <- nrow(dat)
    dat <- dat[dat$SPECIES_CODE %in% species,]
    if(verbose) {cat(nrow(dat),"rows kept out of",totRows,"after filtering by species\n")}
    totRows <- nrow(dat)
    
    if(removeCAN) {
        fpath = system.file("data", "AFSCforeign_hauls.rda", package="nwfscSurvey")
        load(fpath)
        #load("AFSCforeign_hauls.rda")
        foreignHauls = AFSCforeign_hauls
        foreignInd <- !(dat$HAULJOIN %in% foreignHauls$HAULJOIN)
        dat <- dat[foreignInd,]
        if(verbose) {cat(sum(foreignInd),"rows kept (or",sum(!foreignInd),"removed) out of",totRows,"after removing foreign hauls\n")}
        totRows <- nrow(dat)
    }        
    
    dat <- dat[!is.na(dat$LENGTH),]
    dat$Length_cm   <- dat$LENGTH/10
    dat$Weight      <- dat$SP_TOW_WGHT_KG
    dat$Number_fish <- dat$SP_TOW_NUM
    dat$year        <- dat$YEAR

    if(verbose) {cat(nrow(dat),"rows kept out of",totRows,"after removing missing lengths\n")}
    
    return(dat)   
}
