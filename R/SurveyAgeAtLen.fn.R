#' Calculates proportion of age at length and reformats into SS format
#' Uses raw numbers at length, assuming that is a random sample conditioned on length and sex.
#' To use expanded numbers (up to strata areas), set raw=F
#' Only sex codes 1 and 2 and puts males and females on separate lines because the age@L is conditioned on sex (a sample of females of length 25cm, for example)
#' Gender=1: females only. Male values ignored
#' Gender=2: males only. Female values ignored.
#' lgthBins is either the interval between length bins or the actual length bins
#' note that 0 and Inf are tacked on the ends to account for lengths and ages outside the interval. You may want to add these in to first and last bin.
#' I assume all fish are sexed for age data, thus do not apply sex ratios for unsexed fish
#'
#' @param dir directory this is where the output files will be saved
#' @param datAL the biological data frame exctrated from the data warehouse using the PullBio.fn
#' @param datTows the catch data frame extracted from the data warehouse using the PullCatch.fn
#' @param strat.vars the variables used define the stratas. Defaul is bottom depth and latitudes.
#' @param strat.df the created strata matrix with the calculated areas by the createStrataDF.fn function
#' @param lgthBins length bins
#' @param ageBins age bins
#' @param sex sex (0, 1, 2, 3) sex value for Stock Synthesis
#' @param SSout TRUE/FALSE return comps formatted for SS or in a raw form
#' @param meanRatioMethod TRUE/FALSE
#' @param raw TRUE/FALSE, input to define whether or not to expand numbers in the csv file (column header "NumF" and "NumM")
#' @param NAs2zero change NAs to zeros
#' @param month month
#' @param fleet fleet number
#' @param partition partition for Stock Synthesis
#' @param ageErr age error value for Stock Synthesis
#' @param returnSamps TRUE/FALSE stops the function after the sample size is calculated
#' @template printfolder
#' @template verbose
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' @seealso \code{\link{StrataFactors.fn}}
#' @import reshape2

SurveyAgeAtLen.fn <- function(dir = NULL, datAL, datTows, strat.vars = c("Depth_m", "Latitude_dd"), strat.df = NULL, lgthBins = 1, ageBins = 1,
                              sex = 3, SSout = TRUE, meanRatioMethod = TRUE, raw = TRUE, NAs2zero = TRUE, month = "Enter Month", fleet = "Enter Fleet",
                              partition = 0, ageErr = "Enter Age Error", returnSamps = FALSE, printfolder = "forSS3", verbose = TRUE) {

  plotdir <- file.path(dir, printfolder)
  check_dir(plotdir, verbose = verbose)

  totRows <- nrow(datAL)
  datAL <- datAL[!is.na(datAL$Length_cm), ]
  datAL <- datAL[!is.na(datAL$Age), ]
  if (verbose) {
    cat("There are ", nrow(datAL), " records kept out of", totRows, "records after removing missing records.\n")
  }

  row.names(strat.df) <- strat.df[, 1] # put in rownames to make easier to index later
  numStrata <- nrow(strat.df)

  # remove tows that are outside of the stratum
  datAL <- data.frame(datAL, stratum = StrataFactors.fn(datAL, strat.vars, strat.df))
  ind <- !is.na(datAL$stratum)
  datAL <- datAL[ind, ]

  datTows <- data.frame(datTows, stratum = StrataFactors.fn(datTows, strat.vars, strat.df))
  ind <- !is.na(datTows$stratum)
  datTows <- datTows[ind, ]

  ind <- !duplicated(datAL$Trawl_id)
  datB <- datAL[ind, c("Trawl_id", "Weight", strat.vars, "Year")] # individual tow data
  tows <- unique(datAL$Trawl_id)
  Area_Swept_km2 <- Total_fish_number <- Sub_fish_number <- numeric(dim(datB)[1])
  for (i in 1:length(tows)) {
    find <- which(tows[i] == datTows$Trawl_id)
    area <- datTows$Area_swept_ha[find] * 0.01 # km2
    tot.num <- datTows$total_catch_numbers[find]
    sub.num <- datTows$Subsample_count[find]

    find <- which(tows[i] == datB$Trawl_id)
    Area_Swept_km2[find] <- area
    Total_fish_number[find] <- tot.num
    Sub_fish_number[find] <- sub.num
  }
  datB$areaFished <- Area_Swept_km2 # area swept for each tow in km2
  datB$Number_fish <- Total_fish_number
  datB$Sub_number_fish <- Sub_fish_number


  # set up length bins
  if (length(lgthBins) == 1) {
    Lengths <- c(-999, seq(floor(min(datAL$Length_cm)), ceiling(max(datAL$Length_cm)), lgthBins), Inf)
  } else {
    Lengths <- c(-999, lgthBins, Inf)
  }
  # print(Lengths)
  if (length(ageBins) == 1) {
    Ages <- c(-999, seq(floor(min(datAL$Age)), ceiling(max(datAL$Age)), ageBins), Inf)
  } else {
    Ages <- c(-999, ageBins, Inf) # put -999 and Inf on ends because all.inside=T in findInterval below. Treats these as minus and plus groups
  }

  datAL$allLs <- Lengths[findInterval(datAL$Length_cm, Lengths, all.inside = T)]
  datAL$allAs <- Ages[findInterval(datAL$Age, Ages, all.inside = T)]
  # print(head(datAL))

  # first create strata factors
  datB <- data.frame(datB, stratum = StrataFactors.fn(datB, strat.vars, strat.df)) # create a new column for the stratum factor
  numTows <- table(datTows$Year, StrataFactors.fn(datTows, strat.vars, strat.df)) # strata for each individual tow

  # check for unsexed fish
  sexes_present <- unique(datAL$Sex)

  # calculate expansion factor per tow
  # for all sexes
  TdatL.tows <- as.data.frame(table(datAL$Trawl_id))
  datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)), ], TowExpFactorAll = TdatL.tows$Freq)
  datB$TowExpFactorAll <- datB$Number_fish / datB$TowExpFactorAll

  TdatL.tows <- as.data.frame(table(datAL$Trawl_id, datAL$Sex %in% c("U")))
  datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)), ], TowExpFactorU = TdatL.tows$Freq)
  datB$TowExpFactorU <- datB$Number_fish / datB$TowExpFactorU
  datB$TowExpFactorU[datB$TowExpFactorU == Inf] <- NA

  # for females and males only
  TdatL.tows <- as.data.frame(table(datAL$Trawl_id, datAL$Sex %in% c("F", "M")))
  TdatL.tows <- TdatL.tows[TdatL.tows$Var2 == TRUE, ]
  datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)), ], TowExpFactorMF = TdatL.tows$Freq)
  datB$TowExpFactorMF <- datB$Number_fish / datB$TowExpFactorMF
  datB$TowExpFactorMF[datB$TowExpFactorMF == Inf] <- NA

  # find frequency of number of age at lengths
  TdatL.al <- as.data.frame(table(datAL$Trawl_id, datAL$allLs, datAL$allAs)) # first do all age at lengths, unsexed
  names(TdatL.al) <- c("Trawl_id", "allLs", "allAs", "numAll")
  TdatL.al <- TdatL.al[TdatL.al$numAll > 0, ]
  datB <- merge(datB, TdatL.al, by = "Trawl_id", all = T)

  # Females, males, and unsexed
  TdatL.al <- as.data.frame(table(datAL$Trawl_id, datAL$allLs, datAL$allAs, datAL$Sex))
  names(TdatL.al) <- c("Trawl_id", "allLs", "allAs", "Sex", "num")
  TdatL.al <- TdatL.al[TdatL.al$num > 0, ]
  TdatL.al <- split(TdatL.al, TdatL.al$Sex)

  if ("F" %in% sexes_present){
    temp <- TdatL.al[["F"]][, c("Trawl_id", "allLs", "allAs", "num")]
    names(temp) <- c("Trawl_id", "allLs", "allAs", "numF")
    datB <- merge(datB, temp, by = c("Trawl_id", "allLs", "allAs"), all = T)
    datB[is.na(datB$numF), "numF"] <- 0
  }

  if ("M" %in% sexes_present){
    temp <- TdatL.al[["M"]][, c("Trawl_id", "allLs", "allAs", "num")]
    names(temp) <- c("Trawl_id", "allLs", "allAs", "numM")
    datB <- merge(datB, temp, by = c("Trawl_id", "allLs", "allAs"), all = T)
    datB[is.na(datB$numM), "numM"] <- 0
  }

  if ("U" %in% sexes_present){
    temp <- TdatL.al[["U"]][, c("Trawl_id", "allLs", "allAs", "num")]
    names(temp) <- c("Trawl_id", "allLs", "allAs", "numU")
    datB <- merge(datB, temp, by = c("Trawl_id", "allLs", "allAs"), all = T)
    datB[is.na(datB$numU), "numU"] <- 0
  }


  # This sample size is based on haul
  # nSamps <- datB[!duplicated(paste(datB$Trawl_id,datB$allLs)),]
  # nSamps.f <- nSamps[nSamps$numF>0,]
  # nSamps.m <- nSamps[nSamps$numM>0,]
  # nSamps.f <- table(nSamps.f$year,nSamps.f$allLs)
  # nSamps.m <- table(nSamps.m$year,nSamps.m$allLs)
  # if(returnSamps) return(list(nSamps.f,nSamps.m))

  # I am modifying the output sample size to be fish rather than based on haul
  if ("F" %in% sexes_present){
    nSamps.f <- reshape2::dcast(datB, Year ~ allLs, value.var = "numF", sum)
  }
  if ("M" %in% sexes_present){
    nSamps.m <- reshape2::dcast(datB, Year ~ allLs, value.var = "numM", sum)
  }
  if ("U" %in% sexes_present){
    nSamps.u <- reshape2::dcast(datB, Year ~ allLs, value.var = "numU", sum)
  }
  nSamps.all <- reshape2::dcast(datB, Year ~ allLs, value.var = "numAll", sum)

  if (verbose) {
    cat("\nEffective sample size is based on number of fish.\n\n")
  }
  if (returnSamps) {
    return(list(nSamps.f, nSamps.m, nSamps.u))
  }

  if ("U" %in% sexes_present){
    getn.u <- NULL
    for (y in 1:dim(nSamps.u)[1]) {
      for (l in 2:dim(nSamps.u)[2]) {
        if (nSamps.u[y, l] > 0) {
          getn.u <- c(getn.u, nSamps.u[y, l])
        }
      }
    }
  }

  if ("F" %in% sexes_present){
    getn.f <- NULL
    for (y in 1:dim(nSamps.f)[1]) {
      for (l in 2:dim(nSamps.f)[2]) {
        if (nSamps.f[y, l] > 0) {
          getn.f <- c(getn.f, nSamps.f[y, l])
        }
      }
    }
  }

  if ("M" %in% sexes_present){
    getn.m <- NULL
    for (y in 1:dim(nSamps.m)[1]) {
      for (l in 2:dim(nSamps.m)[2]) {
        if (nSamps.m[y, l] > 0) {
          getn.m <- c(getn.m, nSamps.m[y, l])
        }
      }
   }
  }

  getn.all <- NULL
  for (y in 1:dim(nSamps.all)[1]) {
    for (l in 2:dim(nSamps.all)[2]) {
      if (nSamps.all[y, l] > 0) {
        getn.all <- c(getn.all, nSamps.all[y, l])
      }
    }
  }

  # now calculate the expanded lengths per tow
  datB$expAll <- datB$numAll * datB$TowExpFactorAll
  if ("U" %in% sexes_present){
      datB$expU <- datB$numU * datB$TowExpFactorU
  }
  if ("F" %in% sexes_present){
      datB$expF <- datB$numF * datB$TowExpFactorMF
  }
  if ("U" %in% sexes_present){
    datB$expM <- datB$numM * datB$TowExpFactorMF
  }

  # sum over strata within year
  datB.yrLstr <- split(datB, as.character(paste(datB$Year, datB$allLs)))
  datB.yrLstr <- lapply(datB.yrLstr, function(x) {
    split(x, as.character(x$stratum))
  })

  # lengthTotalRatio.fn <- function(x,strat) {
  #    #function to sum lengths within a stratum and a year
  #    #Uses the Total Ratio estimate and Mean ratio estimate
  #    theYear <- unique(x$year)
  #    theStratum <- unique(x$stratum)
  #    if(length(theYear)!=1) stop("there is a problem in length.fn. There should be exactly one year in each list item.\n")
  #    if(length(theStratum)!=1) stop("there is a problem in length.fn. There should be exactly one stratum in each list item.\n")
  #    a.hi <- unlist(lapply(split(x,x$OP_CODE),function(x){x$areaFished[1]}))  #area swept per tow in stratum
  #    a.h <- sum(a.hi)  #total area swept in stratum
  #    A.h <- strat[strat$name==theStratum,"area"]
  #    x$LENGTH_cm <- as.numeric(as.character(x$LENGTH_cm))
  #    noDups <- !duplicated(x$LENGTH_cm)
  #    xcols <- c("Year","stratum") #must be two or more columns to keep the selection a dataframe
  #    lgths <- split(x,x$LENGTH_cm)
  #    LjhU <- unlist(lapply(lgths,function(x){sum(x$expU)}))
  #    x <- data.frame(x[rep(1,length(LjhU)),xcols],area=A.h,areaSwept=a.h,LENGTH_cm=as.numeric(names(LjhU)),LjhU=LjhU,TotalLjhU=A.h*LjhU/a.h)
  #    LjhF <- unlist(lapply(lgths,function(x){sum(x$expF)}))
  #    x <- data.frame(x,LjhF=LjhF,TotalLjhF=A.h*LjhF/a.h)
  #    LjhM <- unlist(lapply(lgths,function(x){sum(x$expM)}))
  #    x <- data.frame(x,LjhM=LjhM,TotalLjhM=A.h*LjhM/a.h)
  #    return(x)
  # }

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
      AjhAll <- unlist(lapply(ages, function(x) {
        sum(x$numAll)
      }))
      x <- data.frame(x[rep(1, length(AjhAll)), xcols], area = A.h, AGE = as.numeric(names(AjhAll)), AjhAll = AjhAll, TotalAjhAll = AjhAll)

      if ("U" %in% sexes_present){
      AjhU <- unlist(lapply(ages, function(x) {
        sum(x$numU)
      }))
      x <- data.frame(x, AjhU = AjhU, TotalAjhU = AjhU)
      }

      if ("F" %in% sexes_present){
      AjhF <- unlist(lapply(ages, function(x) {
        sum(x$numF)
      }))
      x <- data.frame(x, AjhF = AjhF, TotalAjhF = AjhF)
      }

      if ("M" %in% sexes_present){
      AjhM <- unlist(lapply(ages, function(x) {
        sum(x$numM)
      }))
      x <- data.frame(x, AjhM = AjhM, TotalAjhM = AjhM)
      }

    } else {
      AjhAll <- unlist(lapply(ages, function(x) {
        sum(x$expAll / x$areaFished)
      }))
      x <- data.frame(x[rep(1, length(AjhAll)), xcols], area = A.h, AGE = as.numeric(names(AjhAll)), AjhAll = AjhAll / ntows, TotalAjhAll = A.h * AjhAll / ntows)

      AjhU <- unlist(lapply(ages, function(x) {
        sum(x$expU / x$areaFished)
      }))
      x <- data.frame(x, AjhU = AjhU / ntows, TotalAjhU = A.h * AjhU / ntows)

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
      lapply(x, MeanRatio.fn, strat = strat.df, numTows = numTows, raw = raw)
    })
  } else {
    stop("Sorry. Only the mean Ratio Method is implemented")
    #  L.year.str <- lapply(datB.yrstr,function(x){lapply(x,lengthTotalRatio.fn,strat=strat.df)})
  }

  year.fn <- function(x, Ages) { # calculate the age-at-length by year
    theAs <- unlist(lapply(x, function(x) {
      x$AGE
    }))
    allAs <- Ages[findInterval(theAs, Ages, all.inside = T)] # finds the interval that the age falls in (all.inside puts maximum age group into N-1 group, thus I padded with Inf.)
    Lengths <- rep(x[[1]]$allLs[1], length(Ages))
    TotalAjhAll <- unlist(lapply(x, function(x) {
      x$TotalAjhAll
    })) # over strata
    TotalAjAll <- tapply(TotalAjhAll, allAs, sum, na.rm = T) # sum over strata for each age

    if ("U" %in% sexes_present){
      TotalAjhU <- unlist(lapply(x, function(x) {
        x$TotalAjhU
      })) # over strata
      TotalAjU <- tapply(TotalAjhU, allAs, sum, na.rm = T) # sum over strata for each age
    }
    if ("F" %in% sexes_present){
      TotalAjhF <- unlist(lapply(x, function(x) {
        x$TotalAjhF
      }))
      TotalAjF <- tapply(TotalAjhF, allAs, sum, na.rm = T)
    }
    if ("M" %in% sexes_present){
      TotalAjhM <- unlist(lapply(x, function(x) {
        x$TotalAjhM
      }))
      TotalAjM <- tapply(TotalAjhM, allAs, sum, na.rm = T)
    }

    out <- data.frame(Age = Ages, Length = Lengths, propAll = rep(NA, length(Ages)), propU = rep(NA, length(Ages)), propF = rep(NA, length(Ages)), propM = rep(NA, length(Ages)))
    row.names(out) <- out$Age
    out[names(TotalAjAll), "propAll"] <- 100 * TotalAjAll / sum(TotalAjAll, na.rm = T)
    if ("U" %in% sexes_present){
    out[names(TotalAjU), "propU"] <- 100 * TotalAjU / sum(TotalAjU, na.rm = T)
    }
    if ("F" %in% sexes_present){
    out[names(TotalAjF), "propF"] <- 100 * TotalAjF / sum(TotalAjF, na.rm = T)
    }
    if ("M" %in% sexes_present){
    out[names(TotalAjM), "propM"] <- 100 * TotalAjM / sum(TotalAjM, na.rm = T)
    }
    out <- out[-nrow(out), ] # remove last row because Inf and always NA due to inside.all=T
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

  AsAll <- unlist(lapply(AL.year, function(x) {
    x$propAll
  }))
  AsAll[is.na(AsAll)] <- 0
  AsAll <- matrix(AsAll, nrow = length(AL.year), byrow = T, dimnames = list(NULL, paste(rep("All-", length(ages)), ages, sep = "")))
  AsAll[, 2] <- AsAll[, 1] + AsAll[, 2] # add in all ages before the minimum age to the first age bin that we have to specify by ourselves
  numAllzero <- sum(AsAll[, "All--999"])
  AsAll <- AsAll[, -match("All--999", dimnames(AsAll)[[2]])] # remove F0 column

  if ("U" %in% sexes_present){
    AsU <- unlist(lapply(AL.year, function(x) {
      x$propU
    }))
    AsU[is.na(AsU)] <- 0
    AsU <- matrix(AsU, nrow = length(AL.year), byrow = T, dimnames = list(NULL, paste(rep("U", length(ages)), ages, sep = "")))
    AsU[, 2] <- AsU[, 1] + AsU[, 2] # add in all ages before the minimum age to the first age bin that we have to specify by ourselves
    numUzero <- sum(AsU[, "U-999"])
    AsU <- AsU[, -match("U-999", dimnames(AsU)[[2]])] # remove F0 column
  }
  if ("F" %in% sexes_present){
    AsF <- unlist(lapply(AL.year, function(x) {
      x$propF
    }))
    AsF[is.na(AsF)] <- 0
    AsF <- matrix(AsF, nrow = length(AL.year), byrow = T, dimnames = list(NULL, paste(rep("F", length(ages)), ages, sep = "")))
    AsF[, 2] <- AsF[, 1] + AsF[, 2] # add in all ages before the minimum age to the first age bin that we have to specify by ourselves
    numFzero <- sum(AsF[, "F-999"])
    AsF <- AsF[, -match("F-999", dimnames(AsF)[[2]])] # remove F0 column
  }
  if ("M" %in% sexes_present){
    AsM <- unlist(lapply(AL.year, function(x) {
      x$propM
    }))
    AsM[is.na(AsM)] <- 0
    AsM <- matrix(AsM, nrow = length(AL.year), byrow = T, dimnames = list(NULL, paste(rep("M", length(ages)), ages, sep = "")))
    AsM[, 2] <- AsM[, 1] + AsM[, 2] # add in all ages before the minimum age to the first age bin
    numMzero <- sum(AsM[, "M-999"])
    AsM <- AsM[, -match("M-999", dimnames(AsM)[[2]])]
  }

  outAll <- data.frame(
    year = as.numeric(substring(names(AL.year), 1, 4)), month = month, Fleet = fleet, sex = 0, partition = partition, ageErr = ageErr,
    LbinLo = as.numeric(substring(names(AL.year), 6)), LbinHi = as.numeric(substring(names(AL.year), 6)), nSamps = "ENTER", AsAll
  )
  if ("U" %in% sexes_present){
  outU <- data.frame(
    year = as.numeric(substring(names(AL.year), 1, 4)), month = month, Fleet = fleet, sex = 0, partition = partition, ageErr = ageErr,
    LbinLo = as.numeric(substring(names(AL.year), 6)), LbinHi = as.numeric(substring(names(AL.year), 6)), nSamps = "ENTER", AsU
  )
  indZero <- apply(outU[, -c(1:9)], 1, sum) == 0
  outU <- outU[!indZero, ] # remove any rows that have no female observations (they may be there because of male obs)
  outU$nSamps <- getn.u
  rownames(outU) <- paste("U", 1:nrow(outU), sep = "")
  } else {
    outU <- "No unsexed fish"
  }
  if ("F" %in% sexes_present){
  outF <- data.frame(
    year = as.numeric(substring(names(AL.year), 1, 4)), month = month, Fleet = fleet, sex = 1, partition = partition, ageErr = ageErr,
    LbinLo = as.numeric(substring(names(AL.year), 6)), LbinHi = as.numeric(substring(names(AL.year), 6)), nSamps = "ENTER", AsF, AsF
  )
  indZero <- apply(outF[, -c(1:9)], 1, sum) == 0
  outF <- outF[!indZero, ] # remove any rows that have no female observations (they may be there because of male obs)
  outF$nSamps <- getn.f
  rownames(outF) <- paste("F", 1:nrow(outF), sep = "")

  } else {
    outF <- "No Female Fish"
  }
  if ("M" %in% sexes_present){
  outM <- data.frame(
    year = as.numeric(substring(names(AL.year), 1, 4)), month = month, Fleet = fleet, sex = 2, partition = partition, ageErr = ageErr,
    LbinLo = as.numeric(substring(names(AL.year), 6)), LbinHi = as.numeric(substring(names(AL.year), 6)), nSamps = "ENTER", AsM, AsM
  )
  indZero <- apply(outM[, -c(1:9)], 1, sum) == 0
  outM <- outM[!indZero, ] # remove any rows that have no male observations (they may be there because of female obs)
  outM$nSamps <- getn.m
  rownames(outM) <- paste("M", 1:nrow(outM), sep = "")
  } else {
    outM <- "No Male Fish"
  }

  indZero <- apply(outAll[, -c(1:9)], 1, sum) == 0
  outAll <- outAll[!indZero, ] # remove any rows that have no male observations (they may be there because of female obs)
  # Add in the eff N values
  outAll$nSamps <- getn.all
  rownames(outAll) <- paste("All", 1:nrow(outAll), sep = "")

  if (is.null(dir) & verbose) {
    cat("\nDirectory not specified and csv will not be written.\n")
  }
  if (!is.null(dir) & sex != 0) {
    write.csv(outU, file = file.path(plotdir, paste("Survey_CAAL_Unsexed_Bins_", min(lgthBins), "_", max(lgthBins), "_", min(ageBins), "_", max(ageBins), ".csv", sep = "")), row.names = FALSE)
    write.csv(outF, file = file.path(plotdir, paste("Survey_CAAL_Female_Bins_", min(lgthBins), "_", max(lgthBins), "_", min(ageBins), "_", max(ageBins), ".csv", sep = "")), row.names = FALSE)
    write.csv(outM, file = file.path(plotdir, paste("Survey_CAAL_Male_Bins_", min(lgthBins), "_", max(lgthBins), "_", min(ageBins), "_", max(ageBins), ".csv", sep = "")), row.names = FALSE)
  }
  if (!is.null(dir) & sex == 0) {
    write.csv(outAll, file = file.path(plotdir, paste("Survey_CAAL_All_Sexes_Bins_", min(lgthBins), "_", max(lgthBins), "_", min(ageBins), "_", max(ageBins), ".csv", sep = "")), row.names = FALSE)
  }

  #if (verbose) {
  #  if (sex != 0) {
  #    cat("There are", numFzero, "females age 0 to age", ages[2], "minus group that were added into the first age bin\n")
  #    cat("There are", numMzero, "males age 0 to age", ages[2], "minus group that were added into the first age bin\n")
  #    cat("There are", numUzero, "unsexed age 0 to age", ages[2], "minus group that were added into the first age bin\n")
  #  } else {
  #    cat("There are", numAllzero, "unsexed age 0 to age", ages[2], "minus group that were added into the first age bin\n")
  #  }
  #}

  if (sex != 0) {
    return(list(female = outF, male = outM, unsexed = outU))
  } else {
    return(list(all_sexes = outAll))
  }
}
