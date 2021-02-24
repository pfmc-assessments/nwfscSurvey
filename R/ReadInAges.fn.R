#' Reads in the West Coast Trienial survey data and filters the data into what is necessary
#' It reads in data and makes sure only the species necessary are kept
#' may want to keep NA (blank in Excel) to select the zero tows
#' removeCAN is a flag if you want tows in Canadian waters removed
#'
#' Necessary column names
#'    SPECIES_CODE
#'    AGE
#'
#' @param dat data file name
#' @param subset_years specify the years to retain, default is NULL which will provide 1977, alternative input would be 1980:2002 to remove only 1977.
#' @param verbose write out comments
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export

ReadInAges.fn <- function(dat, subset_years = NULL, verbose = TRUE) {
  if (length(subset_years) > 0) {
    dat <- dat[dat$Year %in% subset_years, ]
  }

  totRows <- nrow(dat)
  dat <- dat[!is.na(dat$Age), ]
  dat$Weight <- as.numeric(as.character(dat$Weight_kg))
  dat$year <- as.numeric(as.character(dat$Year))

  if (verbose) {
    cat("There are ", nrow(dat), " of length kept out of", totRows, "after removing fish without ages\n")
  }


  return(dat)
}
