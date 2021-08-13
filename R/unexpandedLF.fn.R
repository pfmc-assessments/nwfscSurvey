#' Creates a matrix of length or age composition data WITHOUT expantion
#' Written by Chantel Wetzel to work with the data warehouse data formatting,
#'
#' @template dir
#' @template bds
#' @param datL *Deprecated* Replaced by bds
#' @param lgthBins User defined age bins to create composition data for.
#' @param agelow *Deprecated* A default value of -1 will be printed in the marginal age composition files
#' @param agehigh *Deprecated* A default value of -1 will be printed in the marginal age composition files
#' @param ageErr Default NA. Value of age error vector to use within Stock Synthesis for the data. This input is only 
#' used when creating age compostion data in the \code{\link{SurveyAFs.fn}}
#' @template sex 
#' @template partition 
#' @template fleet 
#' @param month month the samples were collected
#' @param printfolder folder where the length comps will be saved
#' @param verbose opt to print out message statements
#'
#' @author Chantel Wetzel
#' @export

UnexpandedLFs.fn <- function(dir = NULL, bds, datL = lifecycle::deprecated(), lgthBins = 1, sex = 3, partition = 0, fleet = "Enter Fleet",
                             ageErr = "NA", agelow = lifecycle::deprecated(), agehigh = lifecycle::deprecated(), month = "Enter Month", 
                             printfolder = "forSS", verbose = TRUE) {

  if (lifecycle::is_present(datL)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(datL = )"),
      details = paste0(
        "No longer used. Biological data is now passed via the bds function input."
        )
      )
  }

  if (lifecycle::is_present(agelow)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(agelow = )"),
      details = paste0(
        "This column is now always removed."
        )
      )
  }

  if (lifecycle::is_present(agehigh)) {
    lifecycle::deprecate_stop(
      when = "3.0",
      what = paste0("SurveyLFs.fn(agehigh = )"),
      details = paste0(
        "This column is now always removed."
        )
      )
  }


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

  # Check to see if user is doing ages or lengths
  check <- sum(bds$Length_cm, na.rm = TRUE) == sum(bds$Age, na.rm = TRUE)
  comp.type <- ifelse(check, "Age", "Length")

  totRows <- nrow(bds)
  bds <- bds[!is.na(bds$Length_cm), ]
  if (verbose) {
    cat("There are ", nrow(bds), " records kept out of", totRows, "records after removing missing records.\n")
  }


  # set up length bins
  if (length(lgthBins) == 1) {
    Lengths <- c(-999, seq(floor(min(bds$Length_cm)), ceiling(max(bds$Length_cm)), lgthBins), Inf)
  } else {
    Lengths <- c(-999, lgthBins, Inf)
  }

  # In case there a fish with decimal lengths round them down for processing
  bds$allLs <- Lengths[findInterval(bds$Length_cm, Lengths, all.inside = T)]

  # if there are NA sexes replace them with U
  if (sum(is.na(bds$Sex)) > 0) {
    bds[is.na(bds$Sex), "Sex"] <- "U"
  }

  # Create an assigned sex column
  bds$sex <- bds$Sex

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
  for (y in sort(unique(bds$Year))) {
    # Identify relevant rows
    Which <- which(bds[, "Year"] == y & bds[, "Sex"] %in% sex_out)
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
            Which2 <- Which[which(bds[Which, "allLs"] %in% c(l, -999) & bds[Which, "Sex"] == s)]
          }
          if (l != min(lgthBins)) {
            Which2 <- Which[which(bds[Which, "allLs"] == l & bds[Which, "Sex"] == s)]
          }
          if (l == max(lgthBins)) {
            Which2 <- Which[which(bds[Which, "allLs"] %in% c(l, Inf) & bds[Which, "Sex"] == s)]
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
  if (!"U" %in% sex_out & length(bds[bds$Sex == "U", "Sex"]) > 0) {
    Results <- NULL
    for (y in sort(unique(bds$Year))) {
      # Identify relevant rows
      Which <- which(bds[, "Year"] == y & bds[, "Sex"] == "U")
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
            Which2 <- Which[which(bds[Which, "allLs"] %in% c(l, -999))]
          }
          if (l != min(lgthBins)) {
            Which2 <- Which[which(bds[Which, "allLs"] == l)]
          }
          if (l == max(lgthBins)) {
            Which2 <- Which[which(bds[Which, "allLs"] %in% c(l, Inf))]
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
    agelow = agehigh = -1
    out_comps <- cbind(out[, 1:5], ageErr, agelow, agehigh, out[, 6:dim(out)[2]])
    if (!is.null(out_u)) {
      out_u_comps <- cbind(out_u[, 1:5], ageErr, agelow, agehigh, out_u[, 6:dim(out_u)[2]])
    }
  }

  if (!is.null(dir)) {
    write.csv(out_comps,
      file = file.path(plotdir, paste0(comp.type, "_notExpanded_Sex_", sex, "_bin=", lgthBins[1], "-", max(lgthBins), ".csv")),
      row.names = FALSE
    )
    if (!is.null(out_u)) {
      write.csv(out_u_comps,
        file = file.path(plotdir, paste0(comp.type,"_notExpanded_Sex_0_bin=", lgthBins[1], "-", max(lgthBins), ".csv")),
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
