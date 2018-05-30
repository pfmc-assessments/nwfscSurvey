#' Reads in the West Coast Trienial survey data and filters the data into what is necessary
#' It reads in data and makes sure only the species necessary are kept
#' may want to keep NA (blank in Excel) to select the zero tows
#' removeCAN is a flag if you want tows in Canadian waters removed
#' 
#' Necessary column names
#'    SPECIES_CODE
#'    AGE
#' 
#' @param dir working directory where the results with be saved - not currently used
#' @param dat data file name
#' @param species species name to extract
#' @param survey name of survey data to use. Options are "Tri.Shelf" or "AFSC.Slope"
#' @param subset_years specify the years to retain, default is NULL which will provide 1977, alternative input would be 1980:2002 to remove only 1977.
#' @param removeCAN removes Canadian hauls based on the AFSCforeign_hauls.rda file
#' @param verbose write out comments
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export 

ReadInAges.EWC.fn <- function(dir, dat, species=c(NA), survey, subset_years = NULL, removeCAN=TRUE, verbose=TRUE) {

    dat = dat$Ages[dat$Ages$SURVEY==survey,]

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
        #foreignHauls <- read.csv(paste(directory,"foreign_hauls.csv",sep="\\"))
        foreignInd <- !(dat$HAULJOIN %in% foreignHauls$HAULJOIN)
        dat <- dat[foreignInd,]
        if(verbose) {cat(sum(foreignInd),"rows kept (or",sum(!foreignInd),"removed) out of",totRows,"after removing foreign hauls\n")}
        totRows <- nrow(dat)
    }        
    
    dat <- dat[!is.na(dat$AGE),]
    if(verbose) {cat(nrow(dat),"rows kept out of",totRows,"after removing missing ages\n")}

    dat$Length_cm   <- dat$LENGTH/10
    dat$Weight      <- dat$SP_TOW_WGHT_KG
    dat$Number_fish <- dat$SP_TOW_NUM
    dat$year        <- dat$YEAR
    
    return(dat)   
}
