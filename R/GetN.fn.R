#' Calculate effN input sample sizes
#'
#' @references
#' Stewart, I.J. and O.S. Hamel. 2014.
#' Bootstrapping of sample size for length- or age-composition data used in
#' stock assessment.
#' Canadian Journal of Fishery and Aquatic Science, 71(4): 581--588.
#' [10.1139/cjfas-2013-0289](https://doi.org/10.1139/cjfas-2013-0289).
#'
#' @param dir A file path to the main directory where
#'   `printfolder` will be created. If `NULL`, which is the default,
#'   then no files will be written to the disk.
#' @param dat A `data.frame` of composition data.
#' @param type A string specifying whether doing "length" or "age" that is
#'   used to ensure the sample size is of the correct column and create
#'   the file name of the saved sheet.
#' @param species A string specifying the species group of interest, which
#'   will lead to the use of the correct species-specific value for
#'   the number of unique samples per tow. See the function call for
#'   allowed values, where the default is `"all"`.
#' @param printfolder A string that will be used to create the name of the
#'   folder where files will be saved, i.e., `file.path(dir, printfolder)`.
#' @param output A string, where the default is `NULL`, which returns
#'   only a vector of samples sizes.
#'   `"summary"`, or any other character string, will return
#'   a table of observations by year and sex.
#' @template verbose
#'
#' @author Chantel R. Wetzel
#' @export

GetN.fn <- function(dir = NULL,
                    dat,
                    type = c("length", "age"),
                    species = c(
                      "all",
                      "flatfish",
                      "shelfrock",
                      "sloperock",
                      "thorny",
                      "others"
                    ),
                    printfolder = "forSS",
                    output = NULL,
                    verbose = TRUE) {
  species <- match.arg(species)
  type <- match.arg(type)
  n.unq <- NA
  if (species == "flatfish") {
    n.unq <- 3.09
  }
  if (species == "shelfrock") {
    n.unq <- 2.43
  }
  if (species == "sloperock") {
    n.unq <- 2.43
  }
  if (species == "thorny") {
    n.unq <- 6.91
  }
  if (species == "others") {
    n.unq <- 2.38
  }
  if (species == "all") {
    n.unq <- 2.73
  }

  if (verbose) {
    message("\nThe effN sample size is calculated using the ", species, " multiplier of ", n.unq, ". This number is multiplied by the number of tows in each year.\n")
  }


  if (type == "length") {
    temp <- dat[!is.na(dat$Length_cm), ]
    nSamp <- table(temp$Year, !duplicated(as.character(temp$Trawl_id)))[, "TRUE"]
  }

  if (type == "age") {
    temp <- dat[!is.na(dat$Age), ]
    nSamp <- table(temp$Year, !duplicated(as.character(temp$Trawl_id)))[, "TRUE"]
  }

  if (length(nSamp) == 1) {
    yr <- unique(temp$Year)
  }
  if (length(nSamp) > 1) {
    yr <- as.numeric(names(nSamp))
  }


  n <- floor(n.unq * nSamp)
  fish <- table(temp$Year, temp$Sex)

  if ("F" %in% colnames(fish)) {
    female <- as.numeric(fish[, "F"])
  } else {
    female <- rep(0, dim(fish)[1])
  }

  if ("M" %in% colnames(fish)) {
    male <- as.numeric(fish[, "M"])
  } else {
    male <- rep(0, dim(fish)[1])
  }

  if ("U" %in% colnames(fish)) {
    unsex <- as.numeric(fish[, "U"])
  } else {
    unsex <- rep(0, dim(fish)[1])
  }

  # Add check to cap input N to not be greater than total fish sampled
  ind <- n > (male + female + unsex)
  n[ind] <- male[ind] + female[ind] + unsex[ind]

  if (sum(ind) > 0) {
    if (verbose) {
      cat("\nInput sample size exceded the number of fish for", yr[ind], "and has been capped equal to number of fish.\n")
    }
  }

  samples <- data.frame(
    Year = yr,
    Tows = nSamp,
    All_Fish = female + male + unsex,
    Sexed_Fish = female + male,
    Unsexed_Fish = unsex,
    Sample_Size = n
  )

  # save output as a csv
  if (!is.null(dir)) {
    plotdir <- file.path(dir, printfolder)
    plotdir.isdir <- file.info(plotdir)$isdir
    if (is.na(plotdir.isdir) | !plotdir.isdir) {
      dir.create(plotdir)
    }
    write.csv(samples, file = file.path(plotdir, paste0(type, "_SampleSize.csv", sep = "")), row.names = FALSE)
  }
  if (is.null(output)) {
    return(n)
  } else {
    return(samples)
  }
}
