#' Expands survey length data by the strata and aggregated by length bin and year.
#' Original Version Written by Allan Hicks 16 March 2009
#' Modified by Chantel Wetzel to work with the data warehouse data formatting,
#' add additional options of when to apply the sex ratio, and correct some treatment of unsexed fish
#' weighted by sample size and area
#'
#' @template dir 
#' @template bds
#' @template catch 
#' @param datL *Deprecated* Replaced by bds
#' @param datTows *Deprecated* Replaced by catch
#' @param strat.vars strat.vars The variables used define the stratas. Default is bottom depth and latitudes: c("Depth_m", "Latitude_dd").
#' @template strat.df
#' @param lgthBins User defined length bins to create composition data for. If not specified (default value of 1) the
#' range of lengths to create composition data will be based on range of observerved lengths.
#' @param SSout Default is TRUE. If set to TRUE the output files are formatted for use in Stock Synthesis.
#' @template meanRatioMethod 
#' @template sex 
#' @param NAs2zero *Deprecated*
#' @template sexRatioUnsexed 
#' @param maxSizeUnsexed All sizes below this threshold will assign unsexed fish by sexRatioUnsexed value, fish smaller than this size will have unsexed fish assigned by the calculated sex ratio in the data.
#' @param sexRatioStage Options: 1 or 2. The stage of the expansion to apply the sex ratio. Input either 1 or 2.
#' @template partition 
#' @template fleet 
#' @param agelow *Deprecated* A default value of -1 will be printed in the marginal age composition files
#' @param agehigh *Deprecated* A default value of -1 will be printed in the marginal age composition files
#' @param ageErr Default NA. Value of age error vector to use within Stock Synthesis for the data. This input is only 
#' used when creating age compostion data in the \code{\link{SurveyAFs.fn}}
#' @template nSamps  
#' @template month 
#' @template printfolder 
#' @param remove999 *Deprecated* The -999 column is no longer provided
#' @param outputStage1 Return the first stage expansion - only expanded up to the tow level
#' @template verbose 
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' @seealso \code{\link{StrataFactors.fn}}
#' @seealso \code{\link{SexRatio.fn}}
#' @seealso \code{\link{data_strata_exp}}
#' @seealso \code{\link{year_fn}}

SurveyLFs.fn <- function(dir = NULL, bds, catch, datL = lifecycle::deprecated(), datTows = lifecycle::deprecated(), 
                         strat.vars = c("Depth_m", "Latitude_dd"), 
                         strat.df, lgthBins = 1, SSout = TRUE, meanRatioMethod = TRUE,
                         sex = 3, NAs2zero = lifecycle::deprecated(), sexRatioUnsexed = NA, maxSizeUnsexed = NA, 
                         sexRatioStage = 1, partition = 0, fleet = "Enter Fleet",
                         agelow = "Enter", agehigh = "Enter", ageErr = "Enter", nSamps = NA, 
                         month = "Enter Month", printfolder = "forSS",
                         remove999 = lifecycle::deprecated(), outputStage1 = FALSE, verbose = TRUE) {

  if(missing(strat.df)){
    stop("Must specify the strata via the strat.df function input.")
  }

  if(missing(bds)){
    stop("Must specify the both the bds and catch in the function input.")
  }

  if(missing(catch)){
    stop("Must specify the both the bds and catch in the function input.")
  }

  if (lifecycle::is_present(NAs2zero)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(NAs2zero = )"),
      details = paste0(
        "No longer used. All NAs are converted to 0s."
        )
      )
  }

  if (lifecycle::is_present(datL)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(datL = )"),
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

  if (lifecycle::is_present(remove999)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(remove999 = )"),
      details = paste0(
        "This column is now always removed."
        )
      )
  }

  # Check for the number of tows were fish were observed but not measured
  postows <- catch[which(catch$total_catch_numbers > 0), ]
  find <- !(postows$Trawl_id %in% bds$Trawl_id)
  x <- sum(find)
  missing <- sum(postows[find, "total_catch_numbers"])
  percent <- 100 * round(missing / sum(catch[, "total_catch_numbers"]), 3)
  if (verbose) {
    cat("\nThere are", x, "tows where fish were observed but no lengths/ages taken. 
    These tows contain", missing, "lengths/ages that comprise", percent, "percent of total sampled fish.\n")
  }

  totRows <- nrow(bds)
  bds <- bds[!is.na(bds$Length_cm), ]
  if (verbose) {
    cat("There are ", nrow(bds), " records kept out of", totRows, "records after removing missing records.\n")
  }

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

  # In case there a fish with decimal lengths round them down for processing
  bds$allLs <- Lengths[findInterval(bds$Length_cm, Lengths, all.inside = T)]

  # Create a new column for the stratum factor
  datB <- data.frame(datB, stratum = StrataFactors.fn(datB, strat.vars, strat.df)) 
  # Calc the number of tows in each strata by year for all tows not just positive tows.  
  # This value is then used in the lengthMeanRatio.fn where it is the denominator in the expansion.
  numTows <- table(catch$Year, StrataFactors.fn(catch, strat.vars, strat.df)) # strata for each individual tow

  # Calculate expansion factor per tow
  # for all sexes
  Tbds.tows <- as.data.frame(table(bds$Trawl_id))
  datB <- data.frame(datB[match(as.character(Tbds.tows$Var1), as.character(datB$Trawl_id)), ], true_sub_Allfish = Tbds.tows$Freq)
  datB$TowExpFactorAll <- datB$Number_fish / datB$true_sub_Allfish

  # If there are NA sexes replace them with U
  if (sum(is.na(bds$Sex)) > 0) {
    bds[is.na(bds$Sex), "Sex"] <- "U"
  }

  # for true unsexed fish
  if (sum(bds$Sex == "U") != 0) {
    Tbds.tows <- as.data.frame(table(bds$Trawl_id, bds$Sex %in% c("U")))
    Tbds.tows <- Tbds.tows[Tbds.tows$Var2 == "TRUE", ]
    datB <- data.frame(datB[match(as.character(Tbds.tows$Var1), as.character(datB$Trawl_id)), ], true_sub_Ufish = Tbds.tows$Freq)
    datB$TowExpFactorU <- datB$Number_fish / (datB$Sexed_fish + datB$true_sub_Ufish)
  } else {
    datB$true_sub_Ufish <- 0
    datB$TowExpFactorU <- 1
  }

  # Calculate the tow based expaionsion for females and males combined
  Tbds.tows <- as.data.frame(table(bds$Trawl_id, bds$Sex %in% c("F", "M")))
  Tbds.tows <- Tbds.tows[Tbds.tows$Var2 == "TRUE", ]
  datB <- data.frame(datB[match(as.character(Tbds.tows$Var1), as.character(datB$Trawl_id)), ], TowExpFactorMF = Tbds.tows$Freq)
  # Find the numerator looking where the number of fish = sexed fish when all fish are sampled (e.g., sexed and unsexed in a fully sampled tow)
  # The previous approach expanded sexed fish relative to the full sample size resulting in expansions when there should not have been
  datB <- data.frame(datB[match(as.character(Tbds.tows$Var1), as.character(datB$Trawl_id)), ], true_sub_MFfish = Tbds.tows$Freq)
  if (is.null(datB$true_sub_MFfish)) {
    datB$true_sub_MFfish <- 0
  }
  datB$TowExpFactorMF <- datB$Number_fish / (datB$true_sub_MFfish + datB$true_sub_Ufish)
  datB$TowExpFactorMF[datB$TowExpFactorMF == Inf] <- NA

  # find frequency of number of lengths for all fish
  Tbds.lengths <- as.data.frame(table(bds$Trawl_id, bds$Length_cm))
  names(Tbds.lengths) <- c("Trawl_id", "Length_cm", "numAll")
  Tbds.lengths <- Tbds.lengths[Tbds.lengths$numAll > 0, ]
  datB <- merge(datB, Tbds.lengths, by = "Trawl_id", all = TRUE)

  # Females, males, and true unsexed
  Tbds.lengths <- as.data.frame(table(bds$Trawl_id, bds$Length_cm, bds$Sex))
  names(Tbds.lengths) <- c("Trawl_id", "Length_cm", "Sex", "num")
  Tbds.lengths <- Tbds.lengths[Tbds.lengths$num > 0, ]
  Tbds.lengths <- split(Tbds.lengths, Tbds.lengths$Sex)
  if (!is.null(Tbds.lengths[["F"]])) {
    temp <- Tbds.lengths[["F"]][, c("Trawl_id", "Length_cm", "num")]
    names(temp) <- c("Trawl_id", "Length_cm", "numF")
    datB <- merge(datB, temp, by = c("Trawl_id", "Length_cm"), all = T)
    datB[is.na(datB$numF), "numF"] <- 0
  }
  if (is.null(Tbds.lengths[["F"]])) {
    datB$numF <- 0
  }
  if (!is.null(Tbds.lengths[["M"]])) {
    temp <- Tbds.lengths[["M"]][, c("Trawl_id", "Length_cm", "num")]
    names(temp) <- c("Trawl_id", "Length_cm", "numM")
    datB <- merge(datB, temp, by = c("Trawl_id", "Length_cm"), all = T)
    datB[is.na(datB$numM), "numM"] <- 0
  }
  if (is.null(Tbds.lengths[["M"]])) {
    datB$numM <- 0
  }
  if (!is.null(Tbds.lengths[["U"]])) {
    temp <- Tbds.lengths[["U"]][, c("Trawl_id", "Length_cm", "num")]
    names(temp) <- c("Trawl_id", "Length_cm", "numU")
    datB <- merge(datB, temp, by = c("Trawl_id", "Length_cm"), all = T)
    datB[is.na(datB$numU), "numU"] <- 0
  }
  if (is.null(Tbds.lengths[["U"]])) {
    datB$numU <- 0
  }

  # now calculate the expanded lengths per tow
  datB$expAll <- datB$numAll * datB$TowExpFactorAll
  datB$expF <- datB$numF * datB$TowExpFactorMF
  datB$expM <- datB$numM * datB$TowExpFactorMF
  datB$expU <- datB$numU * datB$TowExpFactorU

  datB$Length_cm <- as.numeric(as.character(datB$Length_cm))
  datB$allLs <- Lengths[findInterval(datB$Length_cm, Lengths, all.inside = TRUE)]


  # Apply the sex ratio to the raw data based on each tow
  if (sexRatioStage == 1) {
    datB <- SexRatio.fn(x = datB, 
                        sexRatioStage = sexRatioStage, 
                        sexRatioUnsexed = sexRatioUnsexed,
                        maxSizeUnsexed = maxSizeUnsexed, 
                        bins = Lengths, 
                        verbose = verbose)
  }

  if (outputStage1) {
    # Rename columns for clarity
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
                       split(x, as.character(x$stratum)) })

  L.year.str <- data_strata_exp(datB.yrstr = datB.yrstr,
                                meanRatioMethod = meanRatioMethod, 
                                strat = strat.df, 
                                numTows = numTows)

  if (sexRatioStage == 2) {
    L.year.str <- SexRatio.fn(x = L.year.str, 
                              sexRatioStage = sexRatioStage, 
                              sexRatioUnsexed = sexRatioUnsexed,
                              maxSizeUnsexed = maxSizeUnsexed, 
                              verbose = verbose)
  }

  L.year <- lapply(L.year.str, year.fn, Lengths = Lengths)

  if (!SSout) {
    return(list(L.year = L.year, L.year.str = L.year.str))
  }

  # Save output as a csv
  # Check to see if user is doing ages or lengths
  check <- sum(bds$Length_cm, na.rm = TRUE) == sum(bds$Age, na.rm = TRUE)
  comp.type <- ifelse(check, "Age", "Length")

  if (length(grep("_samps", names(nSamps))) > 0){
    if (comp.type == "Length") {
      nSamps <- nSamps[[1]]
    } else {
      nSamps <- nSamps[[3]]
    }
  }

  comps_all = getLs(L.year = L.year, sex = -1, month = month, lgthBins = lgthBins,
               fleet = fleet, partition = partition, nSamps = nSamps, verbose = verbose)
  comps_fem = getLs(L.year = L.year, sex = 1, month = month, lgthBins = lgthBins,
               fleet = fleet, partition = partition, nSamps = nSamps, verbose = verbose)
  comps_mal = getLs(L.year = L.year, sex = 2, month = month, lgthBins = lgthBins,
               fleet = fleet, partition = partition, nSamps = nSamps, verbose = verbose)
  comps_both = getLs(L.year = L.year, sex = 3, month = month, lgthBins = lgthBins,
               fleet = fleet, partition = partition, nSamps = nSamps, verbose = verbose)
  comps_unsex = getLs(L.year = L.year, sex = 0, month = month, lgthBins = lgthBins,
               fleet = fleet, partition = partition, nSamps = nSamps, verbose = verbose)

  if (is.null(dir) & verbose) {
    cat("\nDirectory not specified and csv will not be written.\n")
  }
  if (!is.null(dir)) {
    plotdir <- file.path(dir, printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if (is.na(plotdir.isdir) | !plotdir.isdir) {
      dir.create(plotdir)
    }
  }

  if (!is.null(dir)) {

    if (comp.type == "Length") {
       write.csv(comps_all, file = file.path(plotdir, paste("Lengths_All_As_Unsexed_Bins_", min(lgthBins), "-", max(lgthBins), ".csv", sep = "")), row.names = FALSE)
       write.csv(comps_fem, file = file.path(plotdir, paste("Lengths_Females_Bins_", min(lgthBins), "-", max(lgthBins), ".csv", sep = "")), row.names = FALSE)
       write.csv(comps_mal, file = file.path(plotdir, paste("Lengths_Males_Bins_", min(lgthBins), "-", max(lgthBins), ".csv", sep = "")), row.names = FALSE)
       write.csv(comps_both, file = file.path(plotdir, paste("Lengths_Females_Males_Bins_", min(lgthBins), "-", max(lgthBins), ".csv", sep = "")), row.names = FALSE)
       write.csv(comps_unsex, file = file.path(plotdir, paste("Lengths_Unsexed_Bins_", min(lgthBins), "-", max(lgthBins), ".csv", sep = "")), row.names = FALSE)    
    }

    if (comp.type == "Age") {
      agelow <- agehigh <- -1
      comps_all <- cbind(comps_all[, 1:5], ageErr, agelow, agehigh, comps_all[, 6:dim(comps_all)[2]])
      write.csv(comps_all, file = file.path(plotdir, paste("Marginal_Ages_All_As_Unsexed_Bins_", min(lgthBins), "-", max(lgthBins), ".csv", sep = "")), row.names = FALSE)
      comps_fem <- cbind(comps_fem[, 1:5], ageErr, agelow, agehigh, comps_fem[, 6:dim(comps_fem)[2]])
      write.csv(comps_fem, file = file.path(plotdir, paste("Marginal_Ages_Females_Bins_", min(lgthBins), "-", max(lgthBins), ".csv", sep = "")), row.names = FALSE)
      comps_mal <- cbind(comps_mal[, 1:5], ageErr, agelow, agehigh, comps_mal[, 6:dim(comps_mal)[2]])
      write.csv(comps_mal, file = file.path(plotdir, paste("Marginal_Ages_Males_Bins_", min(lgthBins), "-", max(lgthBins), ".csv", sep = "")), row.names = FALSE)
      comps_both <- cbind(comps_both[, 1:5], ageErr, agelow, agehigh, comps_both[, 6:dim(comps_both)[2]])
      write.csv(comps_both, file = file.path(plotdir, paste("Marginal_Ages_Females_Males_Bins_", min(lgthBins), "-", max(lgthBins), ".csv", sep = "")), row.names = FALSE)
      comps_unsex <- cbind(comps_unsex[, 1:5], ageErr, agelow, agehigh, comps_unsex[, 6:dim(comps_unsex)[2]])
      write.csv(comps_unsex, file = file.path(plotdir, paste("Marginal_Ages_Unsexed_Bins_", min(lgthBins), "-", max(lgthBins), ".csv", sep = "")), row.names = FALSE)
    }
    if(verbose){
        cat("\nNOTE: Five files have been saved the the printfolder directory.
        The All As Unsexed file are all lengths/ages treated as unsexed (sex = 0). 
        The Female file are lengths/ages for females only (sex = 1).
        The Male file are lengths/ages for males only (sex = 2).
        The Female Male file are female then male lengths/ages (sex = 3).
        The Unsexed file are fish that were unsexed and no sex ratio was applied to assign them to a sex (sex = 0). If sex ratio was applied this file should have no comps.\n")

    }
  } else {
    if(verbose){
      cat("\nDirectory not specified and csv will not be written.\n")
    }
  }

  if(sex == -1) return( comps_all)
  if(sex ==  0) return( comps_all)  
  if(sex ==  1) return( comps_fem)
  if(sex ==  2) return( comps_mal)
  if(sex ==  3) return( comps_both)
}
