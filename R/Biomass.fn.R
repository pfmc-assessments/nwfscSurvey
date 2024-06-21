#' Calculates design based estimates from survey data for West Coast surveys.
#'
#' @details
#' The design based index is calculated based on the area of the strata and
#' the mean catch by strata. This function returns a list of design-based
#' estimates by strata and estimates combined across stratas by year. This
#' function is designed to work with data frames pulled from the NWFSC
#' data warehouse using [pull_catch()].
#' See: Gunderson, D.R. and Sample, T.M. 1980. Distribution and abundance of rockfish off Washington,
#' Oregon, and California during 1977. Marine Fisheries Review: March - April.
#'
#' @template dir
#' @param dat Data frame of catch data that has been created by the [pull_catch()].
#' @template strat.vars
#' @template strat.df
#' @template printfolder
#' @param outputMedian Logical input to specify whether to output median or the
#' mean biomass estimate. Default `TRUE`.
#' @template month
#' @template fleet
#' @template verbose
#'
#' @returns List of biomass estimates by year, biomass estimates by year and
#' strata, and numbers of fish by year.
#'
#' @author Allan Hicks and Chantel Wetzel
#' @importFrom grDevices dev.off png rgb
#' @importFrom graphics axis legend mtext par plot points segments symbols
#' @importFrom stats optim qnorm sd var
#' @importFrom utils write.csv
#' @export
#'
#' @examples
#' \dontrun{
#' catch <- pull_catch(
#'   common_name = "petrale sole",
#'   survey = "NWFSC.Combo"
#' )
#'
#' strata <- CreateStrataDF.fn(
#'   names = c("shallow_wa", "shallow_or", "shallow_ca", "deep_wa", "deep_or", "deep_ca"),
#'   depths.shallow = c( 55,   55,   55,  183,  183, 183),
#'   depths.deep    = c(183,  183,  183,  549,  549, 549),
#'   lats.south     = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
#'   lats.north     = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0))
#'
#' biommass <- Biomass.fn(
#'   dat = catch,
#'   strat.df = strata
#' )
#'
#' }
#'
Biomass.fn <- function(
  dir = NULL,
  dat,
  strat.vars = c("Depth_m", "Latitude_dd"),
  strat.df,
  printfolder = "forSS3",
  outputMedian = TRUE,
  month = "Enter month",
  fleet = "Enter fleet",
  verbose = TRUE) {

  lifecycle::deprecate_soft(
    when = "2.4",
    what = "nwfscSurvey::Biomass.fn()",
    details = "Please switch to get_design_based()."
  )

  if (is.null(dat$cpue_kg_km2)) stop("There must be a column called cpue_kg_km2 in the dataframe")

  # Calculate the CPUE in terms of number
  dat$cpue_km2_count <- (dat$total_catch_numbers / (0.01 * dat$Area_swept_ha))

  row.names(strat.df) <- strat.df[, 1] # put in rownames to make easier to index later
  numStrata <- nrow(strat.df)

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
  dat <- data.frame(dat, stratum)
  dat.yr <- split(dat, dat$Year)
  dat.stratum <- split(dat, dat$stratum)

  yr.fn <- function(x) {
    x <- split(x, x$stratum)
    namesStrat <- names(x)
    nobs <- unlist(lapply(x, function(x) {
      nrow(x)
    }))
    if (any(nobs <= 1)) {
      if (verbose) {
        cat("*****\nWARNING: At least one stratum in year", x[[1]][1, "year"], "has fewer than one observation.\n*****\n")
      }
    }
    meanCatchRateInStrata <- unlist(lapply(x, function(x) {
      mean(x$cpue_kg_km2)  # * 10000 to match data warehouse spreadsheet
    }))
    varCatchRateInStrata <- unlist(lapply(x, function(x) {
      var(x$cpue_kg_km2)
    }))

    meanNumbersInStrat <- unlist(lapply(x, function(x) {
      mean(x$cpue_km2_count)
    }))
    varNumbersInStrat <- unlist(lapply(x, function(x) {
      var(x$cpue_km2_count)
    }))

    stratStats <- data.frame(
      name = namesStrat,
      area = strat.df[namesStrat, 2],
      ntows = nobs,
      meanCatchRate = meanCatchRateInStrata,
      varCatchRate = varCatchRateInStrata
    )

    stratStats$Bhat <- stratStats$area * stratStats$meanCatchRate
    stratStats$varBhat <- stratStats$varCatchRate * (stratStats$area * stratStats$area) / stratStats$ntows

    stratStats$Nhat <- stratStats$area * meanNumbersInStrat
    stratStats$varNhat <- varNumbersInStrat * (stratStats$area * stratStats$area) / stratStats$ntows
    stratStats
  }

  strata.fn <- function(x) {
    nameStrat <- unique(x$stratum)
    ind <- strat.df$name == nameStrat
    x <- split(x, x$Year)

    namesYear <- names(x)
    nobs <- unlist(lapply(x, function(x) {
      nrow(x)
    }))
    if (any(nobs <= 1)) {
      if (verbose) {
        cat("*****\nWARNING: At least one stratum in year", x[[1]][1, "year"], "has fewer than one observation.\n*****\n")
      }
    }
    meanCatchRateInStrata <- unlist(lapply(x, function(x) {
      mean(x$cpue_kg_km2)  # * 10000 to match data warehouse spreadsheet
    }))
    varCatchRateInStrata <- unlist(lapply(x, function(x) {
      var(x$cpue_kg_km2)
    }))

    meanNumbersInStrat <- unlist(lapply(x, function(x) {
      mean(x$cpue_km2_count)
    }))
    varNumbersInStrat <- unlist(lapply(x, function(x) {
      var(x$cpue_km2_count)
    }))

    stratStats <- data.frame(
      name = namesYear,
      area = strat.df[ind, 2],
      ntows = nobs,
      meanCatchRate = meanCatchRateInStrata,
      varCatchRate = varCatchRateInStrata
    )

    stratStats$Bhat <- stratStats$area * stratStats$meanCatchRate
    stratStats$varBhat <- stratStats$varCatchRate * (stratStats$area * stratStats$area) / stratStats$ntows

    stratStats$Nhat <- stratStats$area * meanNumbersInStrat
    stratStats$varNhat <- varNumbersInStrat * (stratStats$area * stratStats$area) / stratStats$ntows
    stratStats$cv <- sqrt(stratStats$varBhat) / (stratStats$Bhat + 0.000000001)
    stratStats$logVar <- sqrt(log(stratStats$cv^2 + 1))
    stratStats
  }

  yearlyStrataEsts <- lapply(dat.yr, yr.fn)
  names(yearlyStrataEsts) <- paste("Year", names(yearlyStrataEsts), sep = "")

  stratumEsts <- lapply(dat.stratum, strata.fn)

  yrTotal.fn <- function(x) {
    data.frame(
      Bhat = sum(x$Bhat),
      seBhat = sqrt(sum(x$varBhat)),
      cv = sqrt(sum(x$varBhat)) / sum(x$Bhat))
  }

  ests <- as.data.frame(t(as.data.frame(lapply(lapply(yearlyStrataEsts, yrTotal.fn), t)))) # some crazy stuff to put into a dataframe with years as rows
  logVar <- log(ests$cv^2 + 1)
  ln <- data.frame(
    year = substring(row.names(ests), 5),
    meanBhat = ests$Bhat,
    medianBhat = ests$Bhat * exp(-0.5 * logVar),
    SElogBhat = sqrt(logVar)
  )


  df.list <- list()
  df <- list(Strata = yearlyStrataEsts, Total = ests, LNtons = ln)
  if (outputMedian) {
    bio <- data.frame(Year = df$LNtons$year, Season = month, Fleet = fleet, Value = df$LNtons$medianBhat, seLogB = df$LNtons$SElogBhat)
  }
  if (!outputMedian) {
    bio <- data.frame(Year = df$LNtons$year, Season = month, Fleet = fleet, Value = df$LNtons$meanBhat, seLogB = df$LNtons$SElogBhat)
  }

  if (!is.null(dir)) {
    plotdir <- file.path(dir, printfolder)
    check_dir(dir = plotdir, verbose = verbose)
    write.csv(bio, file = file.path(plotdir, paste("design_based_indices.csv", sep = "")), row.names = FALSE)
  }

  df.list$StrataEsts <- stratumEsts
  df.list$All <- df
  df.list$Bio <- bio
  return(df.list)
}
