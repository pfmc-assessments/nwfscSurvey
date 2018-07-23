#' Reads in the compositional survey data and filters the data into what is necessary
#' It reads in catch and station data and makes sure only the species necessary are kept
#' ### may want to keep NA (blank in Excel) to select the zero tows
#' removeCAN is a flag if you want tows in Canadian waters removed
#'    need the file called foreign_hauls.csv
#' 
#' Necessary column names
#'    SPECIES_CODE
#'    LENGTH
#' 
#' @param data data file name
#' @param verbose print out comments
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export 

ReadInLengths.fn <- function(dat, verbose=TRUE) {

    totRows       <- nrow(dat)
    dat           <- dat[!is.na(dat$Length_cm),]
    dat$Weight    <- as.numeric(as.character(dat$Weight_kg))
    dat$year      <- as.numeric(as.character(dat$Year))
    dat$Depth_m   <- as.numeric(as.character(dat$Depth_m))
    dat$Length_cm <- as.numeric(as.character(dat$Length_cm))
    dat$Age       <- as.numeric(as.character(dat$Age))


    if(verbose) {cat("There are ", nrow(dat)," of length kept out of",totRows,"after removing missing lengths\n")}

    
    return(dat)   
}
