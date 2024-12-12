#' Assign sex to unsexed fish based on fish with similar traits
#'
#' @details
#' Assign sex to fish that were sampled but not sexed based on the proportion
#' of like fish that were female out of sexed fish. After assigning unsexed
#' fish to males and females, new sample sizes of each sex are calculated and
#' the expansion factors are augmented.
#' The definition of what classifies as "like fish" depends on the
#' stage of the expansion, see the Details section.
#'
#' @param x A data frame or a nested list depending on `sexRatioStage`
#'   which will dictate where this function is called inside code that
#'   expands the data such as [SurveyLFs.fn()] or [SurveyAFs.fn()].
#' @param sexRatioStage An integer specifying the stage of the expansion in
#'   which the sex ratio should be applied. There is no default value and only
#'   values of `1L`` or `2L` are appropriate.
#' @param sexRatioUnsexed A numerical value within `[0.0, 1.0]` that will be
#'   used as the sex ratio for measured individuals less than `maxSizeUnsexed`.
#'   If `NA_real_`, then the sex ratio for stage-1 expansion will not be
#'   conducted.
#' @param maxSizeUnsexed A numerical value specifying the right side of the
#'   following bin `[0, maxSizeUnsexed]`, where all fish measured in this bin
#'   are assigned a sex based on sexRatioUnsexed.
#'   Fish with a measurement larger than this value will be assigned a sex
#'   based on the calculated sex ratio in the data.
#' @param bins A vector of measurement bins that were used to bin the
#'   measurement of interest. Passing the bins as an argument ensures that
#'   appropriate "like" fish are found, i.e., if a bin is missing from the data
#'   because no fish were measured that fell in that bin the code will be aware
#'   the bin is missing.
#'   TODO: This argument could be avoided if the bins were factors where all
#'         of the levels are documented.
#' @template verbose
#'
#' @author Allan C. Hicks and Chantel R. Wetzel
#' @export
#'
#' @details The sex ratio is calculated as the number of females divided
#' by the sum of the number of females and males. So, the value being
#' reported is actually the proportion of females.
#'
#' `SexRatio.fn()` has different behavior depending on the expansion
#' being performed, which is controlled using the `sexRatioStage` argument.
#'
#' The expansion factors in the returned data frame are unaltered for unsexed
#' fish.
#' TODO: Decide if the previous method is intended or if the expansion factors
#'       should be changed for unsexed fish too but to zero? Also, the stage-2
#'       expansion does alter the number of unsexed fish but the stage-1 does
#'       not.
#' TODO: Think about how the function alters the expansion
#'       sample sizes and did not just report a sex ratio for each stage
#'       that could be used in the expansion functions themselves.
#' TODO: Change the message for stage-2 expansion to better reflect what the
#'       code is doing
#' TODO: align the stage-1 and stage-2 expansion details so they are written in
#'       a similar fashion.
#'
#' * Stage-1 expansion:
#'   * Sex ratio from observations within a tow/measurement bin combination
#'     is applied to all unsexed fish within that same tow/measurement bin;
#'   * Sex ratio from observations within a measurement bin across all years
#'     is applied to all unsexed fish within that same measurement bin that
#'     did not have any sex ratio information available at the tow level;
#'   * Sex ratio from nearby measurement bins across all years is applied
#'     to all unsexed fish within a measurement bin with zero sexed fish.
#'     Here, nearby bins are classified as those that are one measurement unit
#'     smaller and larger than the current bin.
#' * Stage-2 expansion:
#'   * Sex ratio of sexed fish is within a measurement bin is calculated for
#'     every strata/year combination;
#'   * Missing sex ratios for a measurement bin are filled in based on the sex
#'     ratio of all measured fish within that bin across all years and strata;
#'   * Still missing sex ratios for a measurement bin are based on the sex ratio
#'     of sexed fish for near (i.e., plus or minus one) measurement bins away
#'     across all years and strata.
#'
SexRatio.fn <- function(
    x,
    sexRatioStage,
    sexRatioUnsexed,
    maxSizeUnsexed,
    bins = NULL,
    verbose = TRUE) {
  cli::cli_alert_warning(
    "Applying sex ratios is no longer best practice. Please consider turning off
    sex ratio application in SurveyLFs.fn() or using get_expanded_comps()."
  )
  if (sexRatioStage == 1) {
    # incorporate unsexed fish using sex ratios
    if (length(sexRatioUnsexed) == 1 & !is.na(sexRatioUnsexed)) {
      if (verbose) {
        cli::cli_alert_infor("Sex ratio for unsexed fish being applied to the expanded numbers within a tow (stage 1) when possible.
            If no data within a tow for bin then the sex ratio for the bin across all years applied to unsexed fish.
            If no data for that bin across all years then the sex ratio for nearby bins was applied to unsexed fish.")
      }

      x$sexRatio <- x$expF / (x$expF + x$expM)
      # The below line was changed to as.character from as.numeric because it was not finding the correct lengths : CRW
      x$sexRatio[x$Length_cm <= maxSizeUnsexed] <- sexRatioUnsexed

      # in case there are any NA's, we can temporarily put in zeros for calcualtions below
      x[is.na(x$expF), "expF"] <- 0
      x[is.na(x$expM), "expM"] <- 0

      # now fill in any missing ratios with ratios of that bin from other years and strata (can probably be done more efficiently)
      noRatio <- which(is.na(x$sexRatio))
      check <- round(length(noRatio) / length(x$sexRatio), 3)
      if (check > 0.10) {
        if (verbose) {
          cli::cli_alert_info(
            "There are {check} percent of tows with observations that the sex ratio
            will be filled based on other tows. Consider increasing the maxSizeUnsexed
            or create the comps as unsexed."
          )
        }
        if (length(noRatio) > 0) {
          cli::cli_alert_info(
            "These are sex ratios that were filled in using observations from the
            same lengths from different strata and years:"
          )
        }
      }
      for (i in noRatio) {
        inds <- x$allLs == x$allLs[i]
        tmpF <- sum(x$expF[inds])
        tmpM <- sum(x$expM[inds])
        x$sexRatio[i] <- tmpF / (tmpF + tmpM)
        if (verbose) {
          len_check <- x[i, "Length_cm"]
          bin_check <- x[i, "allLs"]
          sex_ratio_check <- x[i, "sexRatio"]
          cli::cli_alert_info(
            "LengthAge: {len_check}, Bin: {bin_check}, Sex Ratio: {sex_ratio_check}"
          )
        }
      }

      noRatio <- which(is.na(x$sexRatio))
      if (length(noRatio) > 0) {
        if (verbose) {
          cli::cli_alert_info(
            "These are sex ratios that were filled in using observations from
            nearby lengths."
          )
        }
      }

      for (i in noRatio) {
        nearLens <- bins[c(which(bins == x$allLs[i]) - 1, which(bins == x$allLs[i]) + 1)]
        inds <- x$allLs %in% nearLens
        tmpF <- sum(x$expF[inds])
        tmpM <- sum(x$expM[inds])
        x$sexRatio[i] <- tmpF / (tmpF + tmpM)
        if (verbose) {
          len_check <- x[i, "Length_cm"]
          bin_check <- x[i, "allLs"]
          sex_ratio_check <- x[i, "sexRatio"]
          cli::cli_alert_info(
            "Length/Age: {len_check}, Bin: {bin_check}, Sex Ratio: {sex_ratio_check}"
          )
        }
      }
      noRatio <- which(is.na(x$sexRatio))
      if (length(noRatio) > 0) {
        if (verbose) {
          cli::cli_alert_info("Some sex ratios were left unknown and omitted")
        }
      }
      if (length(noRatio) == 0) {
        if (verbose) {
          cli::cli_alert_info("Done filling in sex ratios")
        }
      }

      # These lines change to add the actual unsexed fish to the expansion factors -CRW
      x$expF <- x$expF + x$sexRatio * x$expU
      x$expM <- x$expM + (1 - x$sexRatio) * x$expU
      x$expU <- x$expU - x$sexRatio * x$expU - (1 - x$sexRatio) * x$expU
    }
  }

  if (sexRatioStage == 2) {
    if (verbose) {
      cli::cli_alert_info(
        "Sex ratio for unsexed fish being applied to the expanded numbers within a strata and year (stage 2).
         If no data within a strata and year for bin then the sex ratio for the bin across all years and strata applied to unsexed fish."
      )
    }
    # Take everything out of the list into a dataframe
    out <- NULL
    for (a in 1:length(x)) {
      tmp1 <- x[[a]]
      for (b in 1:length(tmp1)) {
        tmp <- tmp1[[b]]
        init <- data.frame(
          Year = tmp$Year,
          stratum = tmp$stratum,
          area = tmp$area,
          LENGTH = tmp$LENGTH,
          TotalLjhAll = tmp$TotalLjhAll,
          TotalLjhF = tmp$TotalLjhF,
          TotalLjhM = tmp$TotalLjhM,
          TotalLjhU = tmp$TotalLjhU
        )
        out <- rbind(out, init)
      }
    }
    # Calculate the sex ratio
    out$sexRatio <- out$TotalLjhF / (out$TotalLjhF + out$TotalLjhM)
    # Fill in the input ratio for small fish
    out$sexRatio[out$LENGTH <= maxSizeUnsexed] <- sexRatioUnsexed

    # Calculate the ratio across years and strata for missing ratios
    noRatio <- which(is.na(out$sexRatio))
    check <- round(length(noRatio) / length(out$sexRatio), 3)
    if (check > 0.10) {
      if (verbose) {
        cli::cli_alert_info(
          "There are {check} percent of tows with observations that the sex ratio
        will be filled based on other tows. Consider increasing the maxSizeUnsexed
        or create the comps as unsexed."
        )
      }
    }

    if (length(noRatio) > 0) {
      if (verbose) {
        cli::cli_alert_info(
          "These are sex ratios that were filled in using observations from the
          same lengths from different strata and years."
        )
      }
    }
    for (i in noRatio) {
      inds <- out$LENGTH == out$LENGTH[i]
      tmpF <- sum(out$TotalLjhF[inds])
      tmpM <- sum(out$TotalLjhM[inds])
      out$sexRatio[i] <- tmpF / (tmpF + tmpM)
      if (verbose) {
        len_check <- out[i, "LENGTH"]
        sex_ratio_check <- round(out[i, "sexRatio"], 3)
        cli::cli_alert_info(
          "Length/Age: {len_check}, Sex Ratio: {sex_ratio_check}"
        )
      }
    }

    # Calculate the ratio based upon near lengths
    noRatio <- which(is.na(out$sexRatio))
    if (length(noRatio) > 0) {
      if (verbose) {
        cli::cli_alert_info(
          "These are sex ratios that were filled in using observations from nearby
          lengths."
        )
      }
      for (i in noRatio) {
        unq.len <- sort(unique(out$LENGTH))
        find <- which(unq.len == out$LENGTH[i])
        if (out$LENGTH[i] == unq.len[length(unq.len)]) {
          nearLens <- which(out$LENGTH == unq.len[find - 1])
        }
        if (out$LENGTH[i] != unq.len[length(unq.len)]) {
          nearLens <- c(which(out$LENGTH == unq.len[find - 1]), which(out$LENGTH == unq.len[find + 1]))
        }
        tmpF <- sum(out$TotalLjhF[nearLens])
        tmpM <- sum(out$TotalLjhM[nearLens])
        out$sexRatio[i] <- tmpF / (tmpF + tmpM)
        if (verbose) {
          len_check <- out[i, "LENGTH"]
          sex_ratio_check <- round(out[i, "sexRatio"], 3)
          cli::cli_alert_info(
            "Length/Age: {len_check}, Sex Ratio: {sex_ratio_check}"
          )
        }
      }
    }

    # out$TotalLjhAll <- out$TotalLjhAll
    out$TotalLjhF <- out$TotalLjhF + out$TotalLjhU * out$sexRatio
    out$TotalLjhM <- out$TotalLjhM + out$TotalLjhU * (1 - out$sexRatio)
    out$TotalLjhU <- round(out$TotalLjhU - out$TotalLjhU * out$sexRatio - out$TotalLjhU * (1 - out$sexRatio), 0)

    # sum over strata within year
    list.yr <- split(out, as.character(out$Year))
    x <- lapply(list.yr, function(x) {
      split(x, as.character(x$stratum))
    })
  }

  return(x)
}
