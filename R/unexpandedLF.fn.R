#' Creates a matrix of length or age composition data WITHOUT expantion
#' Written by Chantel Wetzel to work with the data warehouse data formatting,
#'
#' @param dir directory this is where the output files will be saved
#' @param datL the read in length comps by the PullBio.fn function
#' @param lgthBins length bins
#' @param ageErr Number of ageing error matrix for SS
#' @param agelow age bin for SS (default value of -1)
#' @param agehigh age bin for SS (default value of -1)
#' @param sex (0 = unsexed, 1 = females, 2 = males, 3 = females then males) sex value for Stock Synthesis
#' @param partition partition for Stock Synthesis
#' @param fleet fleet number
#' @param month month the samples were collected
#' @template printfolder
#' @template verbose
#'
#' @author Chantel Wetzel
#' @export

UnexpandedLFs.fn <- function(dir = NULL, datL, lgthBins = 1, sex = 3, partition = 0, fleet = "Enter Fleet",
                             ageErr = "NA", agelow = -1, agehigh = -1, month = "Enter Month", printfolder = "forSS3", verbose = TRUE) {
  plotdir <- file.path(dir, printfolder)
  check_dir(dir = plotdir, verbose = verbose)

  # Check to see if user is doing ages or lengths
  check <- sum(datL$Length_cm, na.rm = TRUE) == sum(datL$Age, na.rm = TRUE)
  comp.type <- ifelse(check, "Age", "Length")

  totRows <- nrow(datL)
  datL <- datL[!is.na(datL$Length_cm), ]
  if (verbose) {
    cat("There are ", nrow(datL), " records kept out of", totRows, "records after removing missing records.\n")
  }


  # set up length bins
  if (length(lgthBins) == 1) {
    Lengths <- c(-999, seq(floor(min(datL$Length_cm)), ceiling(max(datL$Length_cm)), lgthBins), Inf)
  } else {
    Lengths <- c(-999, lgthBins, Inf)
  }

  # In case there a fish with decimal lengths round them down for processing
  datL$allLs <- Lengths[findInterval(datL$Length_cm, Lengths, all.inside = T)]

  # if there are NA sexes replace them with U
  if (sum(is.na(datL$Sex)) > 0) {
    datL[is.na(datL$Sex), "Sex"] <- "U"
  }

  # Create an assigned sex column
  datL$sex <- datL$Sex

  sex_out <- ifelse(sex == 3, "Both",
    ifelse(sex == 2, "M",
      ifelse(sex == 1, "F", "U")
    )
  )
  if (sex_out == "Both") {
    sex_out <- c("F", "M")
  }

  # Create the comps
  Results <- NULL
  for (y in sort(unique(datL$Year))) {
    # Identify relevant rows
    Which <- which(datL[, "Year"] == y & datL[, "Sex"] %in% sex_out)
    # Skip this year unless there are rows
    if (length(Which) > 0) {
      Row <- c(y, length(Which))
      # Loop across F then M
      for (s in sex_out) {
        # Loop across length bins
        for (l in lgthBins)
        {
          # Subset to relevant rows
          if (l == min(lgthBins)) {
            Which2 <- Which[which(datL[Which, "allLs"] %in% c(l, -999) & datL[Which, "Sex"] == s)]
          }
          if (l != min(lgthBins)) {
            Which2 <- Which[which(datL[Which, "allLs"] == l & datL[Which, "Sex"] == s)]
          }
          if (l == max(lgthBins)) {
            Which2 <- Which[which(datL[Which, "allLs"] %in% c(l, Inf) & datL[Which, "Sex"] == s)]
          }
          # Sum to effective sample size by length_bin x Sex x Fleet x Year
          if (length(Which2) == 0) Row <- c(Row, 0)
          if (length(Which2) >= 1) Row <- c(Row, length(Which2))
        }
      }
      # Add to results matrix
      Results <- rbind(Results, Row)
    } # end Which loop
  } # end year loop
  tmp <- data.frame(
    year = Results[, 1],
    month = month,
    fleet = fleet,
    sex = sex,
    partition = partition,
    Nsamp = Results[, 2]
  )
  out <- cbind(tmp, Results[, -c(1:2)])
  colnames(out)[-c(1:6)] <- paste(rep(sex_out, each = length(lgthBins)), lgthBins, sep = "-")

  # Unsexed comps if doing males & females
  # Create the comps
  out_u <- NULL
  if (!"U" %in% sex_out & length(datL[datL$Sex == "U", "Sex"]) > 0) {
    Results <- NULL
    for (y in sort(unique(datL$Year))) {
      # Identify relevant rows
      Which <- which(datL[, "Year"] == y & datL[, "Sex"] == "U")
      # Skip this year unless there are rows
      if (length(Which) > 0) {
        ##### Deal first with "F" or "M" entries
        # Format reference stuff
        Row <- c(y, length(Which))
        # Loop across length bins
        for (l in lgthBins)
        {
          # Subset to relevant rows
          if (l == min(lgthBins)) {
            Which2 <- Which[which(datL[Which, "allLs"] %in% c(l, -999))]
          }
          if (l != min(lgthBins)) {
            Which2 <- Which[which(datL[Which, "allLs"] == l)]
          }
          if (l == max(lgthBins)) {
            Which2 <- Which[which(datL[Which, "allLs"] %in% c(l, Inf))]
          }
          # Sum to effective sample size by length_bin x Sex x Fleet x Year
          if (length(Which2) == 0) Row <- c(Row, 0)
          if (length(Which2) >= 1) Row <- c(Row, length(Which2))
        }
        # Add to results matrix
        Results <- rbind(Results, Row)
      } # end Which loop
    } # end year loop
    tmp <- data.frame(
      year = Results[, 1],
      month = month,
      fleet = fleet,
      sex = 0,
      partition = partition,
      Nsamp = Results[, 2]
    )
    out_u <- cbind(tmp, Results[, -c(1:2)])
    colnames(out_u)[-c(1:6)] <- paste(rep("U", each = length(lgthBins)), lgthBins, sep = "-")
  }

  # Write the files including the -999 column
  if (comp.type == "Length") {
    out_comps <- out
    if (!is.null(out_u)) {
      out_u_comps <- out_u
    }
  }
  if (comp.type == "Age") {
    out_comps <- cbind(out[, 1:5], ageErr, agelow, agehigh, out[, 6:dim(out)[2]])
    if (!is.null(out_u)) {
      out_u_comps <- cbind(out_u[, 1:5], ageErr, agelow, agehigh, out_u[, 6:dim(out_u)[2]])
    }
  }

  if (!is.null(dir)) {
    write.csv(out_comps,
      file = file.path(plotdir, paste0("Survey_notExpanded_", comp.type, "_comp_Sex_", sex, "_bin=", lgthBins[1], "-", max(lgthBins), ".csv")),
      row.names = FALSE
    )
    if (!is.null(out_u)) {
      write.csv(out_u_comps,
        file = file.path(plotdir, paste0("Survey_notExpanded_", comp.type, "_comp_Sex_0_bin=", lgthBins[1], "-", max(lgthBins), ".csv")),
        row.names = FALSE
      )
    }
  }
  out <- list()
  out$comps <- out_comps
  if (!is.null(out_u)) {
    out$comps_u <- out_u_comps
  }

  return(out)
}
