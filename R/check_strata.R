#' Calculate the number of observations by year and strata
#'
#' @details
#' Calculates and returns the total number of tows and
#' positive tows conducted in each strata by year. The
#' selected stratas are used to expand the length and
#' marginal age compositions and to calculate a design
#' based index using the [get_design_based()]. In earlier
#' versions of the code, there needed to be more than one
#' positive observations within each strata to calculate
#' a design based index using [Biomass.fn()]. The new
#' [get_design_based()] function is more robust and will
#' return zeros in each strata-year combination with no
#' observations. However, it can be useful to review how
#' many tows and positive tows are present by year and
#' strata in your data to ensure that the selected strata
#' for expanding the data is reasonable (e.g., avoiding
#' limited observations in large areas).
#'
#'
#' @param data Data frame of catch data that has been created by the [pull_catch()].
#' @template strata
#' @template dir
#' @template printfolder
#' @template verbose
#'
#' @return A matrix with the number of tows and positive tows within each strata
#' by year and the number of positive tows by strata and year.
#'
#' @author Chantel Wetzel
#' @export
#'
#'
check_strata <- function(
    data,
    strata,
    dir = NULL,
    printfolder = "forSS3",
    verbose = TRUE) {
  plotdir <- file.path(dir, printfolder)
  check_dir(dir = plotdir, verbose = verbose)

  # Grab the strata  rownames to index later
  colnames(data) <- tolower(colnames(data))
  original_strata_names <- colnames(strata)
  colnames(strata) <- tolower(colnames(strata))

  # create strata factors
  colnames(strata)[1] <- "stratum"
  stratum <- rep(NA, nrow(data)) # the stratum factor
  for (n in 1:nrow(strata)) {
    ind <- rep(TRUE, nrow(data))
    for (s in c("depth_m", "latitude_dd")) {
      ind <- ind &
        data[, s] >= strata[n, paste(s, ".1", sep = "")] &
        data[, s] < strata[n, paste(s, ".2", sep = "")]
    }
    stratum[ind] <- as.character(strata[n, 1])
  }
  data <- data.frame(data, stratum)

  # add area for each stratum to data frame
  data_joined <- dplyr::left_join(data, strata[, c("stratum", "area")])

  tows_by_strata <- data_joined |>
    dplyr::filter(!is.na(data[, "stratum"])) |>
    dplyr::group_by(year, stratum) |>
    dplyr::summarise(
      tows = dplyr::n(),
      positive_tows = sum(total_catch_numbers > 0)
    ) |>
    dplyr::ungroup()

  if (!is.null(dir)) {
    write.csv(
      x = tows_by_strata,
      file = file.path(plotdir, "strata_observations.csv"),
      row.names = FALSE
    )
  }
  return(tows_by_strata)
}
