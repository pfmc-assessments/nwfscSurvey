#' Expand the data up to the strata area (stage-two) for each   
#' length bin, strata area, and year.
#'
#' @template meanRatioMethod 
#' @param datB.yrstr The datB.yrstr object created by the SurveyLF.fn that contains all data by strata, length, and year
#' @template strat.df
#' @param numTows The total number of tows by year within each strata
#'
#'
#'
data_strata_exp <- function(datB.yrstr, meanRatioMethod, strat, numTows){

  lengthTotalRatio.fn <- function(x, strat) {
    # function to sum lengths within a stratum and a year
    # Uses the Total Ratio estimate and Mean ratio estimate
    theYear <- unique(x$Year)
    theStratum <- unique(x$stratum)
    if (length(theYear) != 1) stop("there is a problem in length.fn. There should be exactly one year in each list item.\n")
    if (length(theStratum) != 1) stop("there is a problem in length.fn. There should be exactly one stratum in each list item.\n")
    # Need to check the below line, not sure if the [1] should be there
    a.hi <- unlist(x$areaFished[1]) # area swept per tow in stratum
    a.h <- sum(a.hi) # total area swept in stratum
    A.h <- strat[strat$name == theStratum, "area"]
    x$LENGTH_cm <- as.numeric(as.character(x$Length_cm))
    xcols <- c("Year", "stratum") # must be two or more columns to keep the selection a dataframe
    lgths <- split(x, x$LENGTH_cm)
    LjhAll <- unlist(lapply(lgths, function(x) {
      sum(x$expAll)
    }))
    out <- data.frame(x[rep(1, length(LjhAll)), xcols],
      area = A.h, areaSwept = a.h,
      LENGTH_cm = as.numeric(names(LjhAll)), LjhAll = LjhAll, TotalLjhAll = round(A.h * LjhAll / a.h)
    )
    LjhF <- unlist(lapply(lgths, function(x) {
      sum(x$expF)
    }))
    out <- data.frame(out, LjhF = LjhF, TotalLjhF = round(A.h * LjhF / a.h))
    LjhM <- unlist(lapply(lgths, function(x) {
      sum(x$expM)
    }))
    out <- data.frame(out, LjhM = LjhM, TotalLjhM = round(A.h * LjhM / a.h))
    LjhU <- unlist(lapply(lgths, function(x) {
      sum(x$expU)
    }))
    out <- data.frame(out, LjhU = LjhU, TotalLjhU = round(A.h * LjhU / a.h))
    return(out)
  }
	
	lengthMeanRatio.fn <- function(x, strat, numTows) {
	  # function to sum lengths within a stratum and a year
	  # Uses the Mean ratio estimate
	  theYear <- unique(x$Year)
	  theStratum <- unique(x$stratum)
	  if (length(theYear) != 1) stop("there is a problem in length.fn. There should be exactly one year in each list item.\n")
	  if (length(theStratum) != 1) stop("there is a problem in length.fn. There should be exactly one stratum in each list item.\n")
	  if (!(as.character(theYear) %in% row.names(numTows))) stop(paste("The year", theYear, "is in the lengths file but is not in the tow file"))
	  ntows <- numTows[as.character(theYear), theStratum]
	  A.h <- strat[strat$name == theStratum, "area"]
	  x$LENGTH_cm <- as.numeric(as.character(x$Length_cm))
	  xcols <- c("Year", "stratum") # must be two or more columns to keep the selection a dataframe
	  lgths <- split(x, x$LENGTH) # splits by length bin for year and stratum
	  LjhAll <- unlist(lapply(lgths, function(x) {
	    sum(x$expAll / x$areaFished)
	  })) # calculated the expansion for unsexed by length bin
	  out <- data.frame(x[rep(1, length(LjhAll)), xcols],
	    area = A.h,
	    LENGTH = as.numeric(names(LjhAll)), TotalLjhAll = A.h * LjhAll / ntows)
	  LjhF <- unlist(lapply(lgths, function(x) {
	    sum(x$expF / x$areaFished)
	  }))
	  out <- data.frame(out, TotalLjhF = A.h * LjhF / ntows)
	  LjhM <- unlist(lapply(lgths, function(x) {
	    sum(x$expM / x$areaFished)
	  }))
	  out <- data.frame(out, TotalLjhM = A.h * LjhM / ntows)
	  LjhU <- unlist(lapply(lgths, function(x) {
	    sum(x$expU / x$areaFished)
	  }))
	  out <- data.frame(out, TotalLjhU = A.h * LjhU / ntows)
	  return(out)
	}

 	if (meanRatioMethod) {
 	  L.year.str <- lapply(datB.yrstr, function(x) {
 	    lapply(x, lengthMeanRatio.fn, strat = strat, numTows) })
 	} else {
 	  L.year.str <- lapply(datB.yrstr, function(x) {
 	    lapply(x, lengthTotalRatio.fn, strat = strat) })
 	}

	return(L.year.str)
} # end function