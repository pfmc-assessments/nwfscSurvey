#' Calculates design based estimates from survey data for West Coast surveys.
#' The design based index is calculated based on the area of the strata and
#' the mean catch by strata.
#' See: Gunderson, D.R. and Sample, T.M. 1980. Distribution and abundance of rockfish off Washington,
#' Oregon, and California during 1977. Marine Fisheries Review: March - April.
#'
#' The variables defining the strata must begin with the name in strat.vars and end with ".1" or ".2" (i.e., Depth_m.1)
#' the strata are assumed to be continuous variables, thus have a lower and upper value defining them. The lower value does not necessarily have to be the same as the previous upper value.
#' the stat.df dataframe is difficult to build up with more than one variable becuase it turns into a design where you have to define all areas, thus repeat the variables for one (like a design)
#'
#' I calculate the variance given stratified sampling theory
#' I work in normal space, then calculate the statistics if B is lognormal
#' This is the Mean Ratio Estimate
#'
#' @param dir directory where the output file will be saved
#' @param dat data-frame of the data that has been by the PullCatch.fn
#' @param strat.vars A vector of the strata variable names (i.e., c("Depth_m","Latitude_dd"))
#' @param strat.df a dataframe with the first column the name of the stratum, the second column the area of the stratum, and the remaining columns are the high and low variables defining the strata created by the CreateStrataDF.fn
#' @param printfolder the folder where files will be saved
#' @param outputMedian T/F output median or the mean biomass estimate
#' @param convert convertion factor for the biomass by area units (e.g., hectare vs km2)
#' @param month month for SS
#' @param fleet fleet number for SS
#' @param verbose opt to print out message statements
#'
#' @author Allan Hicks and Chantel Wetzel
#' @importFrom grDevices dev.off png rgb
#' @importFrom graphics axis legend mtext par plot points segments symbols
#' @importFrom stats optim qnorm sd var
#' @importFrom utils write.csv
#' @export

Biomass.fn <- function(dir = NULL, dat, strat.vars = c("Depth_m", "Latitude_dd"), strat.df, printfolder = "forSS", outputMedian = T,
                       convert = 1, month = NA, fleet = NA, verbose = TRUE) {

  if (is.null(dat$cpue_kg_km2)) stop("There must be a column called cpue_kg_km2 in the dataframe")

  # Calculate the CPUE in terms of nubmer
  dat$cpue_km2_count <- 100 * (dat$total_catch_numbers / (dat$Area_Swept_ha))

  row.names(strat.df) <- strat.df[, 1] # put in rownmaes to make easier to index later
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
      mean(x$cpue_kg_km2)
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
      name = namesStrat, area = strat.df[namesStrat, 2], ntows = nobs,
      meanCatchRate = meanCatchRateInStrata, varCatchRate = varCatchRateInStrata
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
      mean(x$cpue_kg_km2)
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
      name = namesYear, area = strat.df[ind, 2], ntows = nobs,
      meanCatchRate = meanCatchRateInStrata, varCatchRate = varCatchRateInStrata
    )

    stratStats$Bhat <- stratStats$area * stratStats$meanCatchRate
    stratStats$varBhat <- stratStats$varCatchRate * (stratStats$area * stratStats$area) / stratStats$ntows

    stratStats$Nhat <- stratStats$area * meanNumbersInStrat
    stratStats$varNhat <- varNumbersInStrat * (stratStats$area * stratStats$area) / stratStats$ntows
    stratStats$cv <- sqrt(stratStats$varBhat) / (stratStats$Bhat + 0.000000001)
    stratStats$logVar <- sqrt(log(stratStats$cv^2 + 1))
    # stratStats$medianBhat <- stratStats$Bhat*exp(-0.5*stratStats$logVar^2) / convert
    stratStats
  }

  yearlyStrataEsts <- lapply(dat.yr, yr.fn)
  names(yearlyStrataEsts) <- paste("Year", names(yearlyStrataEsts), sep = "")

  stratumEsts <- lapply(dat.stratum, strata.fn)

  yrTotal.fn <- function(x) {
    data.frame(Bhat = sum(x$Bhat), seBhat = sqrt(sum(x$varBhat)), cv = sqrt(sum(x$varBhat)) / sum(x$Bhat))
  }

  ests <- as.data.frame(t(as.data.frame(lapply(lapply(yearlyStrataEsts, yrTotal.fn), t)))) # some crazy stuff to put into a dataframe with years as rows
  logVar <- log(ests$cv^2 + 1)
  ln <- data.frame(
    year = substring(row.names(ests), 5),
    meanBhat = ests$Bhat / convert,
    medianBhat = ests$Bhat * exp(-0.5 * logVar) / convert,
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
    plotdir.isdir <- file.info(plotdir)$isdir
    if (is.na(plotdir.isdir) | !plotdir.isdir) {
      dir.create(plotdir)
    }
    write.csv(bio, file = file.path(plotdir, paste("design_based_indices.csv", sep = "")), row.names = FALSE)
  }

  df.list$StrataEsts <- stratumEsts
  df.list$All <- df
  df.list$Bio <- bio
  return(df.list)
}
