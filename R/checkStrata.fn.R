#' Calculates and returns the total number of tows and 
#' positive tows conducted in each strata by year. The 
#' selected stratas are used to expand the length and 
#' marginal age compositions and to calculate a design
#' based index using the {Biomass.fn} function. 
#'
#' @param dir Directory where the output csv file will be 
#' saved. 
#' @param dat Data-frame of the catch data that has been  
#' created by the {PullCatch.fn} function.
#' @param strat.vars A vector of the strata variable names.   
#' The default input are c("Depth_m","Latitude_dd")) which
#' are the two factors the define a strata area off the coast.
#' @param strat.df Dataframe with the first column the name 
#' of the stratum, the second column the area of the stratum, 
#' and the remaining columns are the high and low variables 
#' defining the strata created by the {CreateStrataDF.fn} function.
#' @param printfolder Folder name where files will be saved. The 
#' default is "forSS" which is also used by other package functions
#' that creates and saves files that are commonly used or reported
#' in Stock Synthesis or the assessment document.
#' @template verbose
#'
#' @author Chantel Wetzel
#' @export
#'
#'
CheckStrata.fn <- function(
  dir = NULL, 
  dat, 
  strat.vars = c("Depth_m", "Latitude_dd"), 
  strat.df, 
  printfolder = "forSS", 
  verbose = TRUE) {

  # Grab the strata  rownmaes to index later
  row.names(strat.df) <- strat.df[, 1] 
  numStrata <- nrow(strat.df)

  # create strata factors
  stratum <- rep(NA, nrow(dat)) # the stratum factor
  for (strat in 1:numStrata) {
    ind <- rep(T, nrow(dat))
    for (i in 1:length(strat.vars)) {
      ind <- ind & dat[, strat.vars[i]] >= strat.df[strat, paste(strat.vars[i], ".1", sep = "")] & dat[, strat.vars[i]] < strat.df[strat, paste(strat.vars[i], ".2", sep = "")]
    }
    stratum[ind] <- as.character(strat.df[strat, 1])
  }

  stratum <- factor(stratum, levels = as.character(strat.df[, 1]))
  catch_strata <- data.frame(dat, stratum)
  year <- split(catch_strata, catch_strata$Year)
  # Create list by strata
  data_stratum <- split(catch_strata, catch_strata$stratum)

  year_fn <- function(x) {
    x <- split(x, x$stratum)
    namesStrat <- names(x)
    nobs <- unlist(lapply(x, function(x) {
      nrow(x)
    }))
    pos <- unlist(lapply(x, function(x) {
      sum(x$total_catch_wt_kg > 0)
    }))
    if (any(nobs <= 1)) {
      if (verbose) {
        cat("*****\nWARNING: At least one stratum in year", x[[1]][1, "year"], "has fewer than one observation.\n*****\n")
      }
    }

    stratStats <- data.frame(name = namesStrat, area = strat.df[namesStrat, 2], ntows = nobs, ptows = pos)
    stratStats
  }

  yearly_strata_ests <- lapply(year, year_fn)

  n_tows <- c(names(yearly_strata_ests)[1], yearly_strata_ests[[1]][, c("ntows")])
  for (a in 2:length(yearly_strata_ests)) {
    n_tows <- rbind(n_tows, c(names(yearly_strata_ests)[a], yearly_strata_ests[[a]]$ntows))
  }
  n_tows <- as.data.frame(n_tows)
  colnames(n_tows) <- c("year", paste0("total_tows_", row.names(strat.df)))
  rownames(n_tows) <- NULL

  positive_tows <- yearly_strata_ests[[1]][, c("ptows")]
  for (a in 2:length(yearly_strata_ests)) {
    positive_tows <- rbind(positive_tows, yearly_strata_ests[[a]]$ptows)
  }
  colnames(positive_tows) <- paste0("positive_tows_", row.names(strat.df))
  rownames(positive_tows) <- NULL

  out <- cbind(n_tows, positive_tows)

  if (!is.null(dir)) {
    plotdir <- file.path(dir, printfolder)
    check_dir(dir = plotdir, verbose = verbose)
    write.csv(
      x = out,
      file = file.path(plotdir, "strata_observations.csv"),
      row.names = FALSE
    )
  }
  return(out)
}
