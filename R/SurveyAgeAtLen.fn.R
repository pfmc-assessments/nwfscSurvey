#' Compiles the lengths and ages into conditional age-at-length format from the  in the biological data for use in Stock Synthesis.
#' Raw numbers at length, data not expanded, assumes that is a random sample conditioned on length and sex.
#' To use expanded numbers (up to strata areas), set raw=FALSE.
#' Only sex codes 1 and 2 and puts males and females on separate lines because the age@L is conditioned on sex (a sample of females of length 25cm, for example)
#' Gender=1: females only. Male values ignored
#' Gender=2: males only. Female values ignored.
#'
#' @template dir 
#' @template bds
#' @template catch
#' @param datAL *Deprecated* Replaced by bds, the biological data frame exctrated from the data warehouse using the PullBio.fn
#' @param datTows *Deprecated* Replaced by catch, the catch data frame extracted from the data warehouse using the PullCatch.fn
#' @param strat.vars the variables used define the stratas. Defaul is bottom depth and latitudes.
#' @param strat.df the created strata matrix with the calculated areas by the createStrataDF.fn function
#' @param lgthBins User defined length bins to create composition data for. If not specified (default value of 1) the
#' range of length to create composition data will be based on range of observerved lengths.
#' @param ageBins User defined age bins to create composition data for. If not specified (default value of 1) the
#' range of ages to create composition data will be based on range of observerved ages.
#' @template sex 
#' @param SSout Default is TRUE. If set to TRUE the output files are formatted for use in Stock Synthesis.
#' @template meanRatioMethod 
#' @param raw TRUE/FALSE, default TRUE. The input to define whether or not to expand numbers in the csv file (column header "NumF" and "NumM")
#' @param NAs2zero *Deprecated* Now will always replace NAs with zeros.
#' @template month 
#' @template fleet 
#' @template partition 
#' @param ageErr Default NA. Value of age error vector to use within Stock Synthesis for the data. This input is only 
#' used when creating age compostion data in the \code{\link{SurveyAFs.fn}} or \code{\link{SurveyAgeAtLen.fn})
#' @param returnSamps TRUE/FALSE, default FALSE. If set to TRUE the function will stop after the sample size is calculated
#' @template printfolder 
#' @templat verbose 
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' @seealso \code{\link{StrataFactors.fn}}
#' @import reshape2
#' @import dplyr
#'
SurveyAgeAtLen.fn <- function(dir = NULL, bds, catch, datAL = lifecycle::deprecated(), datTows = lifecycle::deprecated(), 
                              strat.vars = c("Depth_m", "Latitude_dd"), strat.df = NULL, lgthBins = 1, ageBins = 1,
                              sex = 3, SSout = TRUE, meanRatioMethod = TRUE, raw = TRUE, NAs2zero = lifecycle::deprecated(), 
                              month = "Enter Month", fleet = "Enter Fleet", partition = 0, ageErr = "Enter Age Error", 
                              returnSamps = FALSE, printfolder = "forSS", verbose = TRUE) {

  if (lifecycle::is_present(NAs2zero)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(NAs2zero = )"),
      details = paste0(
        "No longer used. All NAs are converted to 0s."
        )
      )
  }

  if (lifecycle::is_present(datAL)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(datAL = )"),
      details = paste0(
        "No longer used. Biological data is now passed via the bds function input."
        )
      )
  }

  if (lifecycle::is_present(datTows)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(datTows = )"),
      details = paste0(
        "No longer used. Catch data is now passed via the catch function input."
        )
      )
  }

  if (!is.null(dir)) {
    plotdir <- file.path(dir, printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if (is.na(plotdir.isdir) | !plotdir.isdir) {
      dir.create(plotdir)
    }
  }

  totRows <- nrow(bds)
  bds <- bds[!is.na(bds$Length_cm), ]
  bds <- bds[!is.na(bds$Age), ]
  if (verbose) {
    cat("There are ", nrow(bds), " records kept out of", totRows, "records after removing missing records.\n")
  }

  row.names(strat.df) <- strat.df[, 1] # put in rownames to make easier to index later
  numStrata <- nrow(strat.df)

  # remove tows that are outside of the stratum
  bds <- data.frame(bds, stratum = StrataFactors.fn(bds, strat.vars, strat.df))
  ind <- !is.na(bds$stratum)
  bds <- bds[ind, ]

  catch <- data.frame(catch, stratum = StrataFactors.fn(catch, strat.vars, strat.df))
  ind <- !is.na(catch$stratum)
  catch <- catch[ind, ]

  ind <- !duplicated(bds$Trawl_id)
  datB <- bds[ind, c("Trawl_id", strat.vars, "Longitude_dd", "Year")] # individual tow data
  # Calc the area, total fish caught, and sub sampled numbers for the catch data
  catch_sum = as.data.frame(catch %>%
    dplyr::group_by(Trawl_id, .drop = FALSE) %>%
    dplyr::summarise(areaFished = Area_Swept_ha/0.01,
               Number_fish = total_catch_numbers,
               Sub_fish_number = Subsample_count) )
  # Calc the number of actual sexed fish from each positive tow
  bio_sum = as.data.frame(bds %>%
        dplyr::group_by(Trawl_id, .drop = FALSE) %>%
        dplyr::summarise(Sexed_fish = sum(Sex %in% c("F", "M") )))
  # Merge the catch info to the biological data
  datB = dplyr::left_join(datB, catch_sum, by = "Trawl_id")
  datB = dplyr::left_join(datB, bio_sum, by = "Trawl_id")


  # set up length bins
  if (length(lgthBins) == 1) {
    Lengths <- c(-999, seq(floor(min(bds$Length_cm)), ceiling(max(bds$Length_cm)), lgthBins), Inf)
  } else {
    Lengths <- c(-999, lgthBins, Inf)
  }
  # print(Lengths)
  if (length(ageBins) == 1) {
    Ages <- c(-999, seq(floor(min(bds$Age)), ceiling(max(bds$Age)), ageBins), Inf)
  } else {
    Ages <- c(-999, ageBins, Inf) 
    # put -999 and Inf on ends because all.inside=T in findInterval below. Treats these as minus and plus groups
  }

  bds$allLs <- Lengths[findInterval(bds$Length_cm, Lengths, all.inside = T)]
  bds$allAs <- Ages[findInterval(bds$Age, Ages, all.inside = T)]

  # first create strata factors
  datB <- data.frame(datB, stratum = StrataFactors.fn(datB, strat.vars, strat.df)) # create a new column for the stratum factor
  numTows <- table(catch$Year, StrataFactors.fn(catch, strat.vars, strat.df)) # strata for each individual tow

  # calculate expansion factor per tow
  # for all sexes
  TdatL.tows <- as.data.frame(table(bds$Trawl_id))
  datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)), ], TowExpFactorU = TdatL.tows$Freq)
  datB$TowExpFactorU <- datB$Number_fish / datB$TowExpFactorU

  # for females and males only
  TdatL.tows <- as.data.frame(table(bds$Trawl_id, bds$Sex %in% c("F", "M")))
  TdatL.tows <- TdatL.tows[TdatL.tows$Var2 == TRUE, ]
  datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)), ], TowExpFactorMF = TdatL.tows$Freq)
  datB$TowExpFactorMF <- datB$Number_fish / datB$TowExpFactorMF
  datB$TowExpFactorMF[datB$TowExpFactorMF == Inf] <- NA

  # find frequency of number of age at lengths
  TdatL.al <- as.data.frame(table(bds$Trawl_id, bds$allLs, bds$allAs)) # first do all age at lengths, unsexed
  names(TdatL.al) <- c("Trawl_id", "allLs", "allAs", "numU")
  TdatL.al <- TdatL.al[TdatL.al$numU > 0, ]
  datB <- merge(datB, TdatL.al, by = "Trawl_id", all = T)

  # Females and males
  TdatL.al <- as.data.frame(table(bds$Trawl_id, bds$allLs, bds$allAs, bds$Sex))
  names(TdatL.al) <- c("Trawl_id", "allLs", "allAs", "Sex", "num")
  TdatL.al <- TdatL.al[TdatL.al$num > 0, ]
  TdatL.al <- split(TdatL.al, TdatL.al$Sex)
  temp <- TdatL.al[["F"]][, c("Trawl_id", "allLs", "allAs", "num")]
  names(temp) <- c("Trawl_id", "allLs", "allAs", "numF")
  datB <- merge(datB, temp, by = c("Trawl_id", "allLs", "allAs"), all = T)
  datB[is.na(datB$numF), "numF"] <- 0
  temp <- TdatL.al[["M"]][, c("Trawl_id", "allLs", "allAs", "num")]
  names(temp) <- c("Trawl_id", "allLs", "allAs", "numM")
  datB <- merge(datB, temp, by = c("Trawl_id", "allLs", "allAs"), all = T)
  datB[is.na(datB$numM), "numM"] <- 0

  # I am modifying the output sample size to be fish rather than based on haul
  nSamps.f <- reshape2::dcast(datB, Year ~ allLs, value.var = "numF", sum)
  nSamps.m <- reshape2::dcast(datB, Year ~ allLs, value.var = "numM", sum)
  nSamps.u <- reshape2::dcast(datB, Year ~ allLs, value.var = "numU", sum)

  if (verbose) {
    cat("\nEffective sample size is based on number of fish.\n\n")
  }
  if (returnSamps) {
    return(list(nSamps.f, nSamps.m, nSamps.u))
  }

  getn.u <- NULL
  for (y in 1:dim(nSamps.u)[1]) {
    for (l in 2:dim(nSamps.u)[2]) {
      if (nSamps.u[y, l] > 0) {
        getn.u <- c(getn.u, nSamps.u[y, l])
      }
    }
  }

  getn.f <- NULL
  for (y in 1:dim(nSamps.f)[1]) {
    for (l in 2:dim(nSamps.f)[2]) {
      if (nSamps.f[y, l] > 0) {
        getn.f <- c(getn.f, nSamps.f[y, l])
      }
    }
  }

  getn.m <- NULL
  for (y in 1:dim(nSamps.m)[1]) {
    for (l in 2:dim(nSamps.m)[2]) {
      if (nSamps.m[y, l] > 0) {
        getn.m <- c(getn.m, nSamps.m[y, l])
      }
    }
  }

  # now calculate the expanded lengths per tow
  datB$expU <- datB$numU * datB$TowExpFactorU
  datB$expF <- datB$numF * datB$TowExpFactorMF
  datB$expM <- datB$numM * datB$TowExpFactorMF

  # sum over strata within year
  datB.yrLstr <- split(datB, as.character(paste(datB$Year, datB$allLs)))
  datB.yrLstr <- lapply(datB.yrLstr, function(x) {
    split(x, as.character(x$stratum))
  })


  MeanRatio.fn <- function(x, strat, numTows, raw) {
    # function to sum lengths within a stratum and a year
    # Uses the Mean ratio estimate
    theYear <- unique(x$Year)
    theStratum <- unique(x$stratum)
    if (length(theYear) != 1) stop("there is a problem in length.fn. There should be exactly one year in each list item.\n")
    if (length(theStratum) != 1) stop("there is a problem in length.fn. There should be exactly one stratum in each list item.\n")
    if (!(as.character(theYear) %in% row.names(numTows))) stop(paste("The year", theYear, "is in the lengths file but is not in the tow file"))
    ntows <- numTows[as.character(theYear), theStratum]
    A.h <- strat[strat$name == theStratum, "area"]
    xcols <- c("Year", "stratum", "allLs") # must be two or more columns to keep the selection a dataframe
    ages <- split(x, x$allAs)
    if (raw) {
      AjhU <- unlist(lapply(ages, function(x) {
        sum(x$numU)
      }))
      x <- data.frame(x[rep(1, length(AjhU)), xcols], area = A.h, AGE = as.numeric(names(AjhU)), AjhU = AjhU, TotalAjhU = AjhU)
      AjhF <- unlist(lapply(ages, function(x) {
        sum(x$numF)
      }))
      x <- data.frame(x, AjhF = AjhF, TotalAjhF = AjhF)
      AjhM <- unlist(lapply(ages, function(x) {
        sum(x$numM)
      }))
      x <- data.frame(x, AjhM = AjhM, TotalAjhM = AjhM)
    } else {
      AjhU <- unlist(lapply(ages, function(x) {
        sum(x$expU / x$areaFished)
      }))
      x <- data.frame(x[rep(1, length(AjhU)), xcols], area = A.h, AGE = as.numeric(names(AjhU)), AjhU = AjhU / ntows, TotalAjhU = A.h * AjhU / ntows)
      AjhF <- unlist(lapply(ages, function(x) {
        sum(x$expF / x$areaFished)
      }))
      x <- data.frame(x, AjhF = AjhF / ntows, TotalAjhF = A.h * AjhF / ntows)
      AjhM <- unlist(lapply(ages, function(x) {
        sum(x$expM / x$areaFished)
      }))
      x <- data.frame(x, AjhM = AjhM / ntows, TotalAjhM = A.h * AjhM / ntows)
    }
    return(x)
  }

  if (meanRatioMethod) {
    if (verbose) {
      if (raw) cat("\nUsing raw numbers of age-at-length\n\n")
      if (!raw) cat("\nUsing expanded numbers of age-at-length\n\n")
    }
    A.year.L.str <- lapply(datB.yrLstr, function(x) {
      lapply(x, MeanRatio.fn, strat = strat.df, numTows = numTows, raw = raw) })
  } else {
    stop("Sorry, only the mean ratio method is currently implemented.")
  }

  year.fn <- function(x, Ages) { # calculate the age-at-length by year
    theAs <- unlist(lapply(x, function(x) { x$AGE }))

    # finds the interval that the age falls in (all.inside puts maximum age group into N-1 group, thus I padded with Inf.)
    allAs <- Ages[findInterval(theAs, Ages, all.inside = T)] 
    Lengths <- rep(x[[1]]$allLs[1], length(Ages))
    # over strata
    TotalAjhU <- unlist(lapply(x, function(x) { x$TotalAjhU })) 
    TotalAjhF <- unlist(lapply(x, function(x) { x$TotalAjhF }))
    TotalAjhM <- unlist(lapply(x, function(x) { x$TotalAjhM }))
    # sum over strata for each age
    TotalAjU <- tapply(TotalAjhU, allAs, sum, na.rm = T) 
    TotalAjF <- tapply(TotalAjhF, allAs, sum, na.rm = T)
    TotalAjM <- tapply(TotalAjhM, allAs, sum, na.rm = T)
    out <- data.frame(Age = Ages, Length = Lengths, propU = rep(NA, length(Ages)), propF = rep(NA, length(Ages)), propM = rep(NA, length(Ages)))
    row.names(out) <- out$Age
    out[names(TotalAjU), "propU"] <- 100 * TotalAjU / sum(TotalAjU, na.rm = T)
    out[names(TotalAjF), "propF"] <- 100 * TotalAjF / sum(TotalAjF, na.rm = T)
    out[names(TotalAjM), "propM"] <- 100 * TotalAjM / sum(TotalAjM, na.rm = T)
    # remove last row because Inf and always NA due to inside.all=T
    out <- out[-nrow(out), ] 
    return(out)
  }

  AL.year <- list()
  for (i in 1:length(A.year.L.str)) {
    AL.year[[i]] <- year.fn(A.year.L.str[[i]], Ages = Ages)
  }
  names(AL.year) <- names(A.year.L.str)

  if (!SSout) {
    return(list(AL.year = AL.year, A.year.L.str = A.year.L.str))
  }

  # output SS format with gender on separate lines
  ages <- AL.year[[1]][, "Age"]

  AsU <- unlist(lapply(AL.year, function(x) { x$propU }))
  AsU[is.na(AsU)] <- 0
  AsU <- matrix(AsU, nrow = length(AL.year), byrow = T, dimnames = list(NULL, paste(rep("U", length(ages)), ages, sep = "")))
  # add in all ages before the minimum age to the first age bin that we have to specify by ourselves
  AsU[, 2] <- AsU[, 1] + AsU[, 2] 
  numUzero <- sum(AsU[, "U-999"])
  AsU <- AsU[, -match("U-999", dimnames(AsU)[[2]])] # remove F0 column

  AsF <- unlist(lapply(AL.year, function(x) { x$propF }))
  AsF[is.na(AsF)] <- 0
  AsF <- matrix(AsF, nrow = length(AL.year), byrow = T, dimnames = list(NULL, paste(rep("F", length(ages)), ages, sep = "")))
  # add in all ages before the minimum age to the first age bin that we have to specify by ourselves
  AsF[, 2] <- AsF[, 1] + AsF[, 2] 
  numFzero <- sum(AsF[, "F-999"])
  AsF <- AsF[, -match("F-999", dimnames(AsF)[[2]])] # remove F0 column

  AsM <- unlist(lapply(AL.year, function(x) { x$propM }))
  AsM[is.na(AsM)] <- 0
  AsM <- matrix(AsM, nrow = length(AL.year), byrow = T, dimnames = list(NULL, paste(rep("M", length(ages)), ages, sep = "")))
  AsM[, 2] <- AsM[, 1] + AsM[, 2] 
  numMzero <- sum(AsM[, "M-999"])
  AsM <- AsM[, -match("M-999", dimnames(AsM)[[2]])]

  outU <- data.frame(
    year = as.numeric(substring(names(AL.year), 1, 4)), month = month, Fleet = fleet, sex = 0, partition = partition, ageErr = ageErr,
    LbinLo = as.numeric(substring(names(AL.year), 6)), LbinHi = as.numeric(substring(names(AL.year), 6)), nSamps = "ENTER", AsU
  )
  outF <- data.frame(
    year = as.numeric(substring(names(AL.year), 1, 4)), month = month, Fleet = fleet, sex = 1, partition = partition, ageErr = ageErr,
    LbinLo = as.numeric(substring(names(AL.year), 6)), LbinHi = as.numeric(substring(names(AL.year), 6)), nSamps = "ENTER", AsF, AsF
  )
  outM <- data.frame(
    year = as.numeric(substring(names(AL.year), 1, 4)), month = month, Fleet = fleet, sex = 2, partition = partition, ageErr = ageErr,
    LbinLo = as.numeric(substring(names(AL.year), 6)), LbinHi = as.numeric(substring(names(AL.year), 6)), nSamps = "ENTER", AsM, AsM
  )

  indZero <- apply(outU[, -c(1:9)], 1, sum) == 0
  outU <- outU[!indZero, ] # remove any rows that have no female observations (they may be there because of male obs)
  indZero <- apply(outF[, -c(1:9)], 1, sum) == 0
  outF <- outF[!indZero, ] # remove any rows that have no female observations (they may be there because of male obs)
  indZero <- apply(outM[, -c(1:9)], 1, sum) == 0
  outM <- outM[!indZero, ] # remove any rows that have no male observations (they may be there because of female obs)

  # Add in the eff N values
  outU$nSamps <- getn.u
  outF$nSamps <- getn.f
  outM$nSamps <- getn.m

  rownames(outU) <- paste("U", 1:nrow(outU), sep = "")
  rownames(outF) <- paste("F", 1:nrow(outF), sep = "")
  rownames(outM) <- paste("M", 1:nrow(outM), sep = "")

  if (is.null(dir) & verbose) {
    cat("\nDirectory not specified and csv will not be written.\n")
  }
  if (!is.null(dir) & sex != 0) {
    write.csv(outF, file = file.path(plotdir, paste("Survey_CAAL_Female_Bins_", min(lgthBins), "_", max(lgthBins), "_", min(ageBins), "_", max(ageBins), ".csv", sep = "")), row.names = FALSE)
    write.csv(outM, file = file.path(plotdir, paste("Survey_CAAL_Male_Bins_", min(lgthBins), "_", max(lgthBins), "_", min(ageBins), "_", max(ageBins), ".csv", sep = "")), row.names = FALSE)
  }
  if (!is.null(dir) & sex == 0) {
    write.csv(outU, file = file.path(plotdir, paste("Survey_CAAL_Unsexed_Bins_", min(lgthBins), "_", max(lgthBins), "_", min(ageBins), "_", max(ageBins), ".csv", sep = "")), row.names = FALSE)
  }

  if (verbose) {
    if (sex != 0) {
      cat("There are", numFzero, "females age 0 to age", ages[2], "minus group that were added into the first age bin\n")
      cat("There are", numMzero, "males age 0 to age", ages[2], "minus group that were added into the first age bin\n")
    } else {
      cat("There are", numUzero, "unsexed age 0 to age", ages[2], "minus group that were added into the first age bin\n")
    }
  }

  if (sex != 0) {
    return(list(female = outF, male = outM))
  } else {
    return(list(unsexed = outU))
  }
}
