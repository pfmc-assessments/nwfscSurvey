#' Expands the lengths up to the total stratum area then sums over strata
#'
#' @details
#' The original version was written by Allan Hicks 16 March 2009. This function
#' has since been modified by Chantel Wetzel to work with the data warehouse
#' data formatting, add additional options of when to apply the sex ratio, and
#' correct some treatment of unsexed fish weighted by sample size and area.
#'
#' @param dir A file path to an existing directory where you would like to
#'   create a folder to store the output from this function. The default is
#'   `dir = NULL`, which causes the function to not save any files. You can
#'   store the output directly in `dir` if you specify `printfolder = ""`.
#' @param datL A data frame of length-composition data returned from
#'   [PullBio.fn()].
#' @param datTows A data frame of catch data returned from [PullCatch.fn()].
#' @param strat.vars Variables in both `datL` and `datTows` that are used to
#'   define the stratas. Default is bottom depth (m) and latitudes (decimal
#'   degrees), i.e., `c("Depth_m", "Latitude_dd")`.
#' @param strat.df A data frame that defines the strata and provides the
#'   calculated areas for each strata returned from [createStrataDF.fn()].
#' @param lgthBins An integer vector of length bins.
#' @param SSout A logical with the default of `TRUE`. If `TRUE`, the output
#'   is returned in a format that can be directly pasted into an SS3 data file.
#' @param meanRatioMethod A logical with the default of `TRUE`. If `TRUE`, then
#'   the mean ratio is implemented instead of the total ratio. Search the
#'   source code for the equations if more information is needed.
#' @param sex (0, 1, 2, 3). The integer will be used to define the sex column
#'   of the returned input for Stock Synthesis and specifies how the
#'   composition are treated with respect to sex. See the Stock Synthesis
#'   manual for more information. In short, 0 is for unsexed, 1 is females, 2
#'   is males, and 3 is males and females where the sex ratio of the samples is
#'   informative to the model. The default is `3`.
#' @param NAs2zero A logical specifying if `NA`s should be changed to zeros.
#'   The default is `TRUE`.
#' @inheritParams SexRatio.fn
#' @param sexRatioStage (1, 2). The stage of the expansion to apply the sex
#'   ratio. The default is `1`.
#' @param partition,fleet,agelow,agehigh,ageErr,month Each argument requires a
#'   single integer value that will be used to set the associated column of the
#'   returned input for Stock Synthesis. See the Stock Synthesis manual for
#'   more information.
#' @param nSamps A named vector of input or effective sample sizes that will be
#'   used to set the effective sample size of the returned input for Stock
#'   Synthesis. A value must be supplied for every year of data in `datL`.
#' @param printfolder A string that will be appended to `dir`, creating a folder
#'   where the length-composition output will be saved. If specified as `""`,
#'   the output will just be saved directly in `dir`. The default is `"forSS"`.
#' @param remove999 A logical with the default of `TRUE`, which leads to the
#'   output having the 999 column combined with the first length bin.
#' @param outputStage1 A logical specifying if you would like the function to
#'   stop after the end of the first stage of the expansion process and return
#'   output that is not ready for Stock Synthesis. This can be helpful when
#'   wanting output that can be used as input for VAST.
#' @param sum100 A logical value specifying whether to rescale the compositions
#'   to sum to 100. The default is `TRUE`.
#' @template verbose
#'
#' @author Allan Hicks (16 March 2009) and Chantel Wetzel (maintainer)
#' @export
#' @seealso
#' * [StrataFactors.fn()]
#' * [SexRatio.fn()]

SurveyLFs.fn <- function(dir = NULL, datL, datTows, strat.vars = c("Depth_m", "Latitude_dd"), strat.df = NULL, lgthBins = 1, SSout = TRUE, meanRatioMethod = TRUE,
                         sex = 3, NAs2zero = T, sexRatioUnsexed = NA, maxSizeUnsexed = NA, sexRatioStage = 1, partition = 0, fleet = "Enter Fleet",
                         agelow = "Enter", agehigh = "Enter", ageErr = "Enter", nSamps = "Enter Samps", month = "Enter Month", printfolder = "forSS",
                         remove999 = TRUE, outputStage1 = FALSE, sum100 = TRUE, verbose = TRUE) {

  # Check for the number of tows were fish were observed but not measured
  postows <- datTows[which(datTows$total_catch_numbers > 0), ]
  find <- !(postows$Trawl_id %in% datL$Trawl_id)
  x <- sum(find)
  missing <- sum(postows[find, "total_catch_numbers"])
  percent <- 100 * round(missing / sum(datTows[, "total_catch_numbers"]), 3)
  if (verbose) {
    cat("\nThere are", x, "tows where fish were observed but no lengths/ages taken.
        These tows contain", missing, "lengths/ages that comprise", percent, "percent of total sampled fish.\n")
  }

  totRows <- nrow(datL)
  datL <- datL[!is.na(datL$Length_cm), ]
  if (verbose) {
    cat("There are ", nrow(datL), " records kept out of", totRows, "records after removing missing records.\n")
  }


  row.names(strat.df) <- strat.df[, 1] # put in rownames to make easier to index later
  numStrata <- nrow(strat.df)
  ind <- !duplicated(datL$Trawl_id)
  datB <- datL[ind, c("Trawl_id", "Weight", strat.vars, "Longitude_dd", "Year")] # individual tow data
  tows <- unique(datL$Trawl_id)
  Area_Swept <- Total_fish_number <- Sub_fish_number <- Sexed_fish <- numeric(dim(datB)[1])
  for (i in 1:length(tows)) {
    find <- which(tows[i] == datTows$Trawl_id)
    area <- datTows$Area_swept_ha[find] * 0.01 #km2
    #Alternative: * 10000 square meter
    tot.num <- datTows$total_catch_numbers[find]
    sub.num <- datTows$Subsample_count[find]

    find <- which(tows[i] == datL$Trawl_id)
    Sexed_fish[i] <- sum(datL[find, "Sex"] %in% c("F", "M"))

    find <- which(tows[i] == datB$Trawl_id)
    Area_Swept[find] <- area
    Total_fish_number[find] <- tot.num
    Sub_fish_number[find] <- sub.num
  }

  datB$areaFished <- Area_Swept
  datB$Number_fish <- Total_fish_number
  datB$Sub_fish_number <- Sub_fish_number
  datB$Sexed_fish <- Sexed_fish

  # set up length bins
  if (length(lgthBins) == 1) {
    Lengths <- c(-999, seq(floor(min(datL$Length_cm)), ceiling(max(datL$Length_cm)), lgthBins), Inf)
  } else {
    Lengths <- c(-999, lgthBins, Inf)
  }

  # In case there a fish with decimal lengths round them down for processing
  datL$allLs <- Lengths[findInterval(datL$Length_cm, Lengths, all.inside = T)]

  # first create strata factors
  datB <- data.frame(datB, stratum = StrataFactors.fn(datB, strat.vars, strat.df)) # create a new column for the stratum factor
  numTows <- table(datTows$Year, StrataFactors.fn(datTows, strat.vars, strat.df)) # strata for each individual tow

  # calculate expansion factor per tow
  # for all sexes
  TdatL.tows <- as.data.frame(table(datL$Trawl_id))
  # datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)),], TowExpFactorU = TdatL.tows$Freq)
  datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)), ], true_sub_Allfish = TdatL.tows$Freq)
  datB$TowExpFactorAll <- datB$Number_fish / datB$true_sub_Allfish

  # if there are NA sexes replace them with U
  if (sum(is.na(datL$Sex)) > 0) {
    datL[is.na(datL$Sex), "Sex"] <- "U"
  }

  # for true unsexed fish
  if (sum(datL$Sex == "U") != 0) {
    TdatL.tows <- as.data.frame(table(datL$Trawl_id, datL$Sex %in% c("U")))
    TdatL.tows <- TdatL.tows[TdatL.tows$Var2 == "TRUE", ]
    datB <- data.frame(datB[match(as.character(TdatL.tows$Var1), as.character(datB$Trawl_id)), ], true_sub_Ufish = TdatL.tows$Freq)
    datB$TowExpFactorU <- datB$Number_fish / (datB$Sexed_fish + datB$true_sub_Ufish)
  } else {
    datB$true_sub_Ufish <- 0
    datB$TowExpFactorU <- 1
  }


  # for females and males only
  TdatL.tows <- as.data.frame(table(datL$Trawl_id, datL$Sex %in% c("F", "M")))
  TdatL.tows <- TdatL.tows[TdatL.tows$Var2 == "TRUE", ]
  if(dim(TdatL.tows)[1] > 0) {
    datB <- data.frame(
      datB[match(as.character(TdatL.tows$Var1),
      as.character(datB$Trawl_id)), ],
      TowExpFactorMF = TdatL.tows$Freq)
    # Find the numerator looking where the number of fish = sexed fish when all fish are sampled (e.g., sexed and unsexed in a fully sampled tow)
    # The previous approach expanded sexed fish relative to the full sample size resulting in expansions when there should not have been
    datB <- data.frame(datB[match(as.character(TdatL.tows$Var1),
      as.character(datB$Trawl_id)), ],
      true_sub_MFfish = TdatL.tows$Freq)
  }
  if (is.null(datB$true_sub_MFfish)) {
    datB$true_sub_MFfish <- 0
  }
  datB$TowExpFactorMF <- datB$Number_fish / (datB$true_sub_MFfish + datB$true_sub_Ufish)
  datB$TowExpFactorMF[datB$TowExpFactorMF == Inf] <- NA

  # find frequency of number of lengths for all fish
  TdatL.lengths <- as.data.frame(table(datL$Trawl_id, datL$Length_cm))
  names(TdatL.lengths) <- c("Trawl_id", "Length_cm", "numAll")
  TdatL.lengths <- TdatL.lengths[TdatL.lengths$numAll > 0, ]
  datB <- merge(datB, TdatL.lengths, by = "Trawl_id", all = T)

  # Females, males, and true unsexed
  TdatL.lengths <- as.data.frame(table(datL$Trawl_id, datL$Length_cm, datL$Sex))
  names(TdatL.lengths) <- c("Trawl_id", "Length_cm", "Sex", "num")
  TdatL.lengths <- TdatL.lengths[TdatL.lengths$num > 0, ]
  TdatL.lengths <- split(TdatL.lengths, TdatL.lengths$Sex)
  if (!is.null(TdatL.lengths[["F"]])) {
    temp <- TdatL.lengths[["F"]][, c("Trawl_id", "Length_cm", "num")]
    names(temp) <- c("Trawl_id", "Length_cm", "numF")
    datB <- merge(datB, temp, by = c("Trawl_id", "Length_cm"), all = T)
    datB[is.na(datB$numF), "numF"] <- 0
  }
  if (is.null(TdatL.lengths[["F"]])) {
    datB$numF <- 0
  }
  if (!is.null(TdatL.lengths[["M"]])) {
    temp <- TdatL.lengths[["M"]][, c("Trawl_id", "Length_cm", "num")]
    names(temp) <- c("Trawl_id", "Length_cm", "numM")
    datB <- merge(datB, temp, by = c("Trawl_id", "Length_cm"), all = T)
    datB[is.na(datB$numM), "numM"] <- 0
  }
  if (is.null(TdatL.lengths[["M"]])) {
    datB$numM <- 0
  }
  if (!is.null(TdatL.lengths[["U"]])) {
    temp <- TdatL.lengths[["U"]][, c("Trawl_id", "Length_cm", "num")]
    names(temp) <- c("Trawl_id", "Length_cm", "numU")
    datB <- merge(datB, temp, by = c("Trawl_id", "Length_cm"), all = T)
    datB[is.na(datB$numU), "numU"] <- 0
  }
  if (is.null(TdatL.lengths[["U"]])) {
    datB$numU <- 0
  }

  # now calculate the expanded lengths per tow
  datB$expAll <- datB$numAll * datB$TowExpFactorAll
  datB$expF <- datB$numF * datB$TowExpFactorMF
  datB$expM <- datB$numM * datB$TowExpFactorMF
  datB$expU <- datB$numU * datB$TowExpFactorU

  datB$Length_cm <- as.numeric(as.character(datB$Length_cm))
  datB$allLs <- Lengths[findInterval(datB$Length_cm, Lengths, all.inside = T)]

  # Apply the sex ratio to the raw data based on each tow
  if (sexRatioStage == 1) {
    datB <- SexRatio.fn(
      x = datB, sexRatioStage = sexRatioStage, sexRatioUnsexed = sexRatioUnsexed,
      maxSizeUnsexed = maxSizeUnsexed, bins = Lengths, verbose = verbose
    )
  }


  if (outputStage1) {
    stageOne <- datB[, c(
      "Trawl_id", "Year", "allLs", "Length_cm", "Depth_m", "Latitude_dd", "Longitude_dd", "stratum", "areaFished",
      "Number_fish", "true_sub_Ufish", "true_sub_MFfish", "expAll", "expF", "expM", "expU"
    )]
    # Rename columns
    names(stageOne)[names(stageOne) == "allLs"] <- "Bins"
    names(stageOne)[names(stageOne) == "expAll"] <- "Nall"
    names(stageOne)[names(stageOne) == "expF"] <- "Nf"
    names(stageOne)[names(stageOne) == "expM"] <- "Nm"
    names(stageOne)[names(stageOne) == "expU"] <- "Nu"
    names(stageOne)[names(stageOne) == "true_sub_Ufish"] <- "subsample_U"
    names(stageOne)[names(stageOne) == "true_sub_MFfish"] <- "subsample_MF"

    if (verbose) {
      cat("\nNOTE: Stage 1 expansion returned by the function. Composition file not written for SS.\n")
    }
    return(stageOne)
  }


  # sum over strata within year
  datB.yrstr <- split(datB, as.character(datB$Year))
  datB.yrstr <- lapply(datB.yrstr, function(x) {
    split(x, as.character(x$stratum))
  })


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
      LENGTH = as.numeric(names(LjhAll)), TotalLjhAll = round(A.h * LjhAll / ntows)
    )
    LjhF <- unlist(lapply(lgths, function(x) {
      sum(x$expF / x$areaFished)
    }))
    out <- data.frame(out, TotalLjhF = round(A.h * LjhF / ntows))
    LjhM <- unlist(lapply(lgths, function(x) {
      sum(x$expM / x$areaFished)
    }))
    out <- data.frame(out, TotalLjhM = round(A.h * LjhM / ntows))
    LjhU <- unlist(lapply(lgths, function(x) {
      sum(x$expU / x$areaFished)
    }))
    out <- data.frame(out, TotalLjhU = round(A.h * LjhU / ntows))
    return(out)
  }

  if (meanRatioMethod) {
    L.year.str <- lapply(datB.yrstr, function(x) {
      lapply(x, lengthMeanRatio.fn, strat = strat.df, numTows)
    })
  } else {
    L.year.str <- lapply(datB.yrstr, function(x) {
      lapply(x, lengthTotalRatio.fn, strat = strat.df)
    })
  }

  if (sexRatioStage == 2) {
    L.year.str <- SexRatio.fn(
      x = L.year.str, sexRatioStage = sexRatioStage, sexRatioUnsexed = sexRatioUnsexed,
      maxSizeUnsexed = maxSizeUnsexed, verbose = verbose
    )
  }

  year.fn <- function(x, Lengths) { # calculate the LFs by year
    theLs.yr <- unlist(lapply(x, function(x) {
      as.numeric(as.character(x$LENGTH))
    }))
    TotalLjhAll <- unlist(lapply(x, function(x) {
      as.numeric(as.character(x$TotalLjhAll))
    }))
    TotalLjhF <- unlist(lapply(x, function(x) {
      as.numeric(as.character(x$TotalLjhF))
    }))
    TotalLjhM <- unlist(lapply(x, function(x) {
      as.numeric(as.character(x$TotalLjhM))
    }))
    TotalLjhU <- unlist(lapply(x, function(x) {
      as.numeric(as.character(x$TotalLjhU))
    }))

    # Finds the interval that the length falls in and floors it (so 23.2 would be in 23 if 23 was a level in Lengths,
    # all.inside puts maximum age group into N-1 group, thus I padded with Inf.)
    allLs <- Lengths[findInterval(theLs.yr, Lengths, all.inside = T)]
    TotalLjAll <- tapply(TotalLjhAll, allLs, sum, na.rm = T)
    TotalLjF <- tapply(TotalLjhF, allLs, sum, na.rm = T)
    TotalLjM <- tapply(TotalLjhM, allLs, sum, na.rm = T)
    TotalLjU <- tapply(TotalLjhU, allLs, sum, na.rm = T)

    out <- data.frame(
      Length = Lengths, TotalLjAll = rep(NA, length(Lengths)), TotalLjF = rep(NA, length(Lengths)),
      TotalLjM = rep(NA, length(Lengths)), TotalLjU = rep(NA, length(Lengths))
    )
    row.names(out) <- out$Length
    if (sum100) {
      out[names(TotalLjAll), "TotalLjAll"] <- 100 * TotalLjAll / sum(TotalLjAll, na.rm = T)
      out[names(TotalLjF), "TotalLjF"] <- 100 * TotalLjF / (sum(TotalLjF, na.rm = T) + sum(TotalLjM, na.rm = T))
      out[names(TotalLjM), "TotalLjM"] <- 100 * TotalLjM / (sum(TotalLjF, na.rm = T) + sum(TotalLjM, na.rm = T))
      out[names(TotalLjU), "TotalLjU"] <- 100 * TotalLjU / (sum(TotalLjU, na.rm = T))
    } else {
      out[names(TotalLjAll), "TotalLjAll"] <- TotalLjAll
      out[names(TotalLjF), "TotalLjF"] <- TotalLjF
      out[names(TotalLjM), "TotalLjM"] <- TotalLjM
      out[names(TotalLjU), "TotalLjU"] <- TotalLjU
    }
    out <- out[-nrow(out), ] # remove last row because Inf and always NA due to inside.all=T
    return(out)
  }

  L.year <- lapply(L.year.str, year.fn, Lengths = Lengths)

  if (!SSout) {
    return(list(L.year = L.year, L.year.str = L.year.str))
  }

  # otherwise return SS output for gender type
  if (sex == 0) {
    sex.name <- c("U", "U")
    lgths <- as.character(L.year[[1]]$Length)
    Ls <- unlist(lapply(L.year, function(x) {
      c(x$TotalLjAll, x$TotalLjAll)
    }))
  }

  if (sex == 1) {
    # females only
    sex.name <- c("F", "F")
    lgths <- as.character(L.year[[1]]$Length)
    Ls <- unlist(lapply(L.year, function(x) {
      c(x$TotalLjF, x$TotalLjF)
    }))
  }

  if (sex == 2) {
    # males only
    sex.name <- c("M", "M")
    lgths <- as.character(L.year[[1]]$Length)
    Ls <- unlist(lapply(L.year, function(x) {
      c(x$TotalLjM, x$TotalLjM)
    }))
  }

  if (sex == 3) {
    # females then males
    sex.name <- c("F", "M")
    lgths <- as.character(L.year[[1]]$Length)
    Ls <- unlist(lapply(L.year, function(x) {
      c(x$TotalLjF, x$TotalLjM)
    }))
  }

  if (NAs2zero) {
    Ls[is.na(Ls)] <- 0
  }
  Ls <- matrix(Ls,
    nrow = length(L.year), byrow = T,
    dimnames = list(NULL, paste(c(rep(sex.name[1], length(lgths)), rep(sex.name[2], length(lgths))), lgths, sep = ""))
  )

  if (!"Enter Samps" %in% nSamps & dim(Ls)[1] != length(nSamps)) {
    stop("The length of of annual sample sizes input to the function do not match the years with length/age data.")
  }

  out <- data.frame(
    year = as.numeric(names(L.year)),
    month = month,
    fleet = fleet,
    sex = rep(sex, length(L.year)),
    partition = partition,
    InputN = nSamps,
    Ls
  )

  if (is.na(sexRatioUnsexed)) {
    # unsexed fish
    lgths <- as.character(L.year[[1]]$Length)
    Ls <- unlist(lapply(L.year, function(x) {
      c(x$TotalLjU, x$TotalLjU)
    }))
    if (NAs2zero) {
      Ls[is.na(Ls)] <- 0
    }
    Ls <- matrix(Ls,
      nrow = length(L.year), byrow = T,
      dimnames = list(NULL, paste(c(rep("U", length(lgths)), rep("U", length(lgths))), lgths, sep = ""))
    )
    out2 <- data.frame(
      year = as.numeric(names(L.year)), month = month, fleet = fleet, sex = rep(0, length(L.year)),
      partition = partition, InputN = nSamps, Ls
    )
  }

  usableOut <- out
  if (sex == 3 && is.na(sexRatioUnsexed)) {
    usableOut2 <- out2
  }

  # Save output as a csv
  # Check to see if user is doing ages or lengths
  check <- sum(datL$Length_cm, na.rm = TRUE) == sum(datL$Age, na.rm = TRUE)
  comp.type <- ifelse(check, "Age", "Length")

  plotdir <- file.path(dir, printfolder)
  check_dir(dir = plotdir, verbose = verbose)

  # Write the files including the -999 column
  if (comp.type == "Length") {
    out.comps <- out
  }
  if (comp.type == "Age") {
    out.comps <- cbind(out[, 1:5], ageErr, agelow, agehigh, out[, 6:dim(out)[2]])
  }

  if (!is.null(dir)) {
    write.csv(out.comps, file = file.path(plotdir, paste("Survey_Sex", sex, "_Bins_-999_", max(lgthBins), "_", comp.type, "Comps.csv", sep = "")), row.names = FALSE)
  }
  # If doing sexed comps but no sex ratio is applied to unsexed fish, here are those unsexed fish
  if (sex == 3 && is.na(sexRatioUnsexed)) {
    if (comp.type == "Length") {
      out.comps <- out2
    }
    if (comp.type == "Age") {
      out.comps <- cbind(out2[, 1:5], ageErr, agelow, agehigh, out2[, 6:dim(out2)[2]])
    }
    if (!is.null(dir)) {
      write.csv(out.comps, file = file.path(plotdir, paste("Survey_Sex_Unsexed_Bins_-999_", max(lgthBins), "_", comp.type, "Comps.csv", sep = "")), row.names = FALSE)
    }
  }


  # Write female and then male comp data
  if (sex == 3) {
    usableOut[, paste0("F", min(lgthBins))] <- usableOut[, paste0("F", min(lgthBins))] + usableOut$F.999
    usableOut[, paste0("M", min(lgthBins))] <- usableOut[, paste0("M", min(lgthBins))] + usableOut$M.999
    usableOut <- usableOut[, -which(names(usableOut) %in% c("F.999", "M.999"))]
    if (comp.type == "Age") {
      usableOut <- cbind(usableOut[, 1:5], ageErr, agelow, agehigh, usableOut[, 6:dim(usableOut)[2]])
    }
    if (is.null(dir) & verbose) {
      cat("\nDirectory not specified and csv will not be written.\n")
    }
    if (!is.null(dir)) {
      write.csv(usableOut, file = file.path(plotdir, paste("Survey_Sex", sex, "_Bins_", min(lgthBins), "_", max(lgthBins), "_", comp.type, "Comps.csv", sep = "")), row.names = FALSE)
    }
    if (is.na(sexRatioUnsexed)) {
      usableOut2[, paste0("U", min(lgthBins))] <- usableOut2[, paste0("U", min(lgthBins))] + usableOut2$U.999
      usableOut2 <- usableOut2[, -which(names(usableOut2) %in% c("U.999", "U.999.1"))]
      if (comp.type == "Age") {
        usableOut2 <- cbind(usableOut2[, 1:5], ageErr, agelow, agehigh, usableOut2[, 6:dim(usableOut2)[2]])
      }
      if (is.null(dir) & verbose) {
        cat("\nDirectory not specified and csv will not be written.\n")
      }
      if (!is.null(dir)) {
        write.csv(usableOut2, file = file.path(plotdir, paste("Survey_Sex_Unsexed_Bins_", min(lgthBins), "_", max(lgthBins), "_", comp.type, "Comps.csv", sep = "")), row.names = FALSE)
      }
    }
  }

  # Write male comp data only
  if (sex == 2) {
    usableOut[, paste0("M", min(lgthBins))] <- usableOut[, paste0("M", min(lgthBins))] + usableOut$M.999
    usableOut <- usableOut[, -which(names(usableOut) %in% c("M.999", "M.999.1"))]
    if (comp.type == "Age") {
      usableOut <- cbind(usableOut[, 1:5], ageErr, agelow, agehigh, usableOut[, 6:dim(usableOut)[2]])
    }
    if (is.null(dir) & verbose) {
      cat("\nDirectory not specified and csv will not be written.\n")
    }
    if (!is.null(dir)) {
      write.csv(usableOut, file = file.path(plotdir, paste("Survey_Sex", sex, "_Bins_", min(lgthBins), "_", max(lgthBins), "_", comp.type, "Comps.csv", sep = "")), row.names = FALSE)
    }
  }

  # Write female comp data only
  if (sex == 1) {
    usableOut[, paste0("F", min(lgthBins))] <- usableOut[, paste0("F", min(lgthBins))] + usableOut$F.999
    usableOut <- usableOut[, -which(names(usableOut) %in% c("F.999", "F.999.1"))]
    if (comp.type == "Age") {
      usableOut <- cbind(usableOut[, 1:5], ageErr, agelow, agehigh, usableOut[, 6:dim(usableOut)[2]])
    }
    if (is.null(dir) & verbose) {
      cat("\nDirectory not specified and csv will not be written.\n")
    }
    if (!is.null(dir)) {
      write.csv(usableOut, file = file.path(plotdir, paste("Survey_Sex", sex, "_Bins_", min(lgthBins), "_", max(lgthBins), "_", comp.type, "Comps.csv", sep = "")), row.names = FALSE)
    }
  }

  # Write unsexed comp data
  if (sex == 0) {
    usableOut[, paste0("U", min(lgthBins))] <- usableOut[, paste0("U", min(lgthBins))] + usableOut$U.999
    usableOut <- usableOut[, -which(names(usableOut) %in% c("U.999", "U.999.1"))]
    if (comp.type == "Age") {
      usableOut <- cbind(usableOut[, 1:5], ageErr, agelow, agehigh, usableOut[, 6:dim(usableOut)[2]])
    }
    if (is.null(dir) & verbose) {
      cat("\nDirectory not specified and csv will not be written.\n")
    }
    if (!is.null(dir)) {
      write.csv(usableOut, file = file.path(plotdir, paste("Survey_Sex", sex, "_Bins_", min(lgthBins), "_", max(lgthBins), "_", comp.type, "Comps.csv", sep = "")), row.names = FALSE)
    }
  }

  if (verbose && !is.null(dir)) {
    cat("\nNOTE: Files have been saved the the printfolder directory.
        The first file has the 999 column showing fish smaller or younger than the initial bin.
        Check to make sure there is not a large number of fish smaller or younger than the initial bin.
        The second file has combined the 999 with the first bin and is ready for use in SS.\n")
  }

  if (!remove999) {
    return(out)
  }
  if (remove999) {
    return(usableOut)
  }
}
