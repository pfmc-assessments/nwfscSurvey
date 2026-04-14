#' Calculate the number of observations by year and strata
#'
#' @description
#' Provides a summary of total and positive tows across different strata and years
#' to help evaluate the suitability of the selected stratification.
#'
#' @details
#' Calculates and returns the total number of tows and
#' positive tows conducted in each strata by year. The
#' selected strata are used to expand the length and
#' marginal age compositions and to calculate a design-based
#' index using [get_design_based()].
#'
#' In earlier versions of the code, more than one positive observation
#' within each strata was required to calculate a design-based index.
#' The current [get_design_based()] function is more robust and returns
#' zeros in strata-year combinations with no observations. However,
#' reviewing the frequency of tows and positive tows is recommended
#' to ensure that the selected strata are reasonable (e.g., avoiding
#' limited observations in large areas).
#'
#' @inheritParams pull_catch
#' @param data Data frame of catch data created by [pull_catch()].
#' @param strata A data frame that defines the strata and provides the
#'   calculated areas for each strata returned from [create_strata()].
#' @param printfolder Deprecated with {nwfscSurvey} 2.8.0 to give users greater
#'   control on where to save files. A string that will be appended to `dir`,
#'   creating a folder where the output will be saved.
#'
#' @return A matrix containing the total number of tows and positive tows
#'   cross-tabulated by year and strata.
#'
#' @author Chantel Wetzel
#' @export
#' @family helper functions
check_strata <- function(
  data,
  strata,
  dir = NULL,
  printfolder = lifecycle::deprecated(),
  verbose = TRUE
) {
  if (lifecycle::is_present(printfolder)) {
    lifecycle::deprecate_warn(
      when = "2.8.0",
      what = "nwfscSurvey::check_strata(printfolder =)"
    )
  }
  plotdir <- file.path(dir)
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
      positive_tows = sum(cpue_kg_km2 > 0, na.rm = TRUE)
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
