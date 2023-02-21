#' Creates a matrix of length or age composition data WITHOUT expantion
#' Written by Chantel Wetzel to work with the data warehouse data formatting,
#'
#' @param dir directory this is where the output files will be saved
#' @param datL the read in length comps by the PullBio.fn function
#' @param lgthBins A vector on length bins or age bins to group the data for Stock Synthsis
#' @param ageErr Number of ageing error matrix for Stock Synthesis
#' @param agelow age bin for Stock Synthesis (default value of -1)
#' @param agehigh age bin for Stock Synthesis (default value of -1)
#' @param sex Deprecated with {nwfscSurvey} 2.2.1 (February 2023). The function will now output
#' female/male and unsexed compositions if present in the data frame datL 
#' @param partition partition for Stock Synthesis
#' @param fleet fleet number
#' @param month month the samples were collected used by Stock Synthesis to determine the length/age estimate to compare to.
#' @param two_sex_model Default TRUE. If TRUE and unsexed composition data are present the unsexed comps
#' will be output in the format needed for a two-sex model in Stock Synthesis.
#' @param printfolder folder where the length comps will be saved
#' @template verbose
#'
#' @author Chantel Wetzel
#' @export

UnexpandedLFs.fn <- function(dir = NULL, datL, lgthBins = 1, sex = lifecycle::deprecated(), partition = 0, fleet = "Enter Fleet",
  ageErr = "NA", agelow = -1, agehigh = -1, month = "Enter Month", two_sex_model = TRUE, printfolder = "forSS", verbose = TRUE) {

  if (lifecycle::is_present(sex)) {
    lifecycle::deprecate_warn(
      when = "2.2.1",
      what = "nwfscSurvey::UnexpandedAFs.fn(sex =)"
    )
  }

  plotdir <- file.path(dir, printfolder)
  check_dir(dir = plotdir, verbose = verbose)

  # Automatically find the needed columns
  sexn <- grep("sex", colnames(datL), ignore.case = TRUE, value = TRUE)
  lenn <- grep("length", colnames(datL), ignore.case = TRUE, value = TRUE)
  agen <- grep("age", colnames(datL), ignore.case = TRUE, value = TRUE)
  yearn <- grep("year", colnames(datL), ignore.case = TRUE, value = TRUE)

  # Check to see if user is doing ages or lengths
  check <- sum(datL[, lenn], na.rm = TRUE) == sum(datL[, agen], na.rm = TRUE)
  comp_type <- ifelse(check, "Age", "Length")

  totRows <- nrow(datL)
  keep <- !is.na(datL[, lenn])
  datL <- datL[keep, ]
  if (verbose & length(keep) != totRows) {
    cat("There are ", nrow(datL) - length(keep), " records kept out of", totRows, "records with missing length or ages.\n")
  }

  # set up length bins
  if (length(lgthBins) == 1) {
    Lengths <- c(-999, seq(floor(min(datL[,lenn])), ceiling(max(datL[, lenn])), lgthBins), Inf)
  } else {
    Lengths <- c(-999, lgthBins, Inf)
  }

  # In case there a fish with decimal lengths round them down for processing
  datL$allLs <- Lengths[findInterval(datL[, lenn], Lengths, all.inside = T)]

  # if there are NA sexes replace them with U
  if (sum(is.na(datL[, sexn])) > 0) {
    datL[is.na(datL[, sexn]), sexn] <- "U"
  }

  # Create an assigned sex column
  datL$use_sex <- datL[, sexn]

  # sex_out <- ifelse(sex == 3, "Both",
  #   ifelse(sex == 2, "M",
  #     ifelse(sex == 1, "F", "U")
  #   )
  # )
  # if (sex_out == "Both") {
  #   sex_out <- c("F", "M")
  # }

  # Create the comps
  Results <- out <- NULL
  for (y in sort(unique(datL[, yearn]))) {
    # Identify relevant rows
    Which <- which(datL[, yearn] == y & datL[, sexn] %in% c("F", "M"))
    # Skip this year unless there are rows
    if (length(Which) > 0) {
      Row <- c(y, length(Which))
      # Loop across F then M
      for (s in c("F", "M")) {
        # Loop across length bins
        for (l in lgthBins)
        {
          # Subset to relevant rows
          if (l == min(lgthBins)) {
            Which2 <- Which[which(datL[Which, "allLs"] %in% c(l, -999) & datL[Which, sexn] == s)]
          }
          if (l != min(lgthBins)) {
            Which2 <- Which[which(datL[Which, "allLs"] == l & datL[Which, sexn] == s)]
          }
          if (l == max(lgthBins)) {
            Which2 <- Which[which(datL[Which, "allLs"] %in% c(l, Inf) & datL[Which, sexn] == s)]
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
  
  if(!is.null(Results)){
    Results <- as.data.frame(Results)
    tmp <- data.frame(
      year = Results[, 1],
      month = month,
      fleet = fleet,
      sex = 3,
      partition = partition,
      Nsamp = Results[, 2]
    )
    out <- cbind(tmp, Results[, -c(1:2)])
    colnames(out)[-c(1:6)] <- c(
      paste(rep("F", each = length(lgthBins)), lgthBins, sep = "-"),
      paste(rep("M", each = length(lgthBins)), lgthBins, sep = "-"))
  }


  # Create unsexed comps if present in the data
  out_u <- NULL
  if (length(datL[datL[, sexn] == "U", sexn]) > 0) {
    Results <- NULL
    for (y in sort(unique(datL[, yearn]))) {
      # Identify relevant rows
      Which <- which(datL[, yearn] == y & datL[, sexn] == "U")
      # Skip this year unless there are rows
      if (length(Which) > 0) {
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
    Results <- as.data.frame(Results)
    tmp <- data.frame(
      year = Results[, 1],
      month = month,
      fleet = fleet,
      sex = 0,
      partition = partition,
      Nsamp = Results[, 2]
    )

    if (two_sex_model){
      out_u <- cbind(tmp, Results[, -c(1:2)], Results[, -c(1:2)])
    } else {
      out_u <- cbind(tmp, Results[, -c(1:2)])
    }    
    colnames(out_u)[-c(1:6)] <- paste(rep("U", each = length(lgthBins)), lgthBins, sep = "-")
  }

  # Write the files including the -999 column
  if (comp_type == "Length") {
    if (!is.null(out)) {    
      out_comps <- out
    } else {
      out_comps <- NULL
    }
    if (!is.null(out_u)) {
      out_u_comps <- out_u
    } 
  }

  if (comp_type == "Age") {
    if (!is.null(out)) {
      out_comps <- cbind(out[, 1:5], ageErr, agelow, agehigh, out[, 6:dim(out)[2]])
    } else {
      out_comps <- NULL
    }
    if (!is.null(out_u)) {
      out_u_comps <- cbind(out_u[, 1:5], ageErr, agelow, agehigh, out_u[, 6:dim(out_u)[2]])
    }
  }

  if (!is.null(dir)) {
    if (!is.null(out_comps)) {
      write.csv(out_comps,
        file = file.path(plotdir, paste0("unexpanded_", comp_type, "_comp_sex_3_bin=", lgthBins[1], "-", max(lgthBins), ".csv")),
        row.names = FALSE
      )
    }
    if (!is.null(out_u)) {
      write.csv(out_u_comps,
        file = file.path(plotdir, paste0("unexpanded_", comp_type, "_comp_sex_0_bin=", lgthBins[1], "-", max(lgthBins), ".csv")),
        row.names = FALSE
      )
    }
  }
  
  out <- list()
  if (!is.null(out_comps)) {
    out$sexed <- out_comps
  }
  if (!is.null(out_u)) {
    out$unsexed <- out_u_comps
  }

  return(out)
}
