#' Pull NWFSC Hook-and-Line Survey data from pinned data list on posit connect
#'
#' This function can be used to return pinned data for NWFSC Hook-and-Line Survey.
#' If a `common_name` is provided the data will be formatted and filtered for
#' that species.  A list is returned with catch and biological data.
#'
#' @param return_positive_sites_only Logical that specifies whether data from sites
#'   that have observed common_name at least once across the time series are
#'   returned.The default is \code{TRUE}.
#' @inheritParams pull_catch
#'
#' @return Returns a list of hook-and-line catch and biological data
#' @author Chantel Wetzel
#' @family data pulling functions
#' @export
#'
#' @import cli
#'
pull_hkl_cache <- function(
  years = c(1970, 2050),
  common_name = NULL,
  dir = NULL,
  return_positive_sites_only = TRUE,
  verbose = TRUE
) {
  board <- pins::board_url(
    c(
      dataset = "https://connect.fisheries.noaa.gov/nwfsc_hkl_survey_data/"
    )
  )
  data <- suppressMessages(pins::pin_read(
    board,
    "dataset"
  ))
  year_range <- years[1]:years[2]
  hkl_data <- data |>
    dplyr::filter(year %in% year_range)
  catch_data <- format_hkl_catch_data(
    data = hkl_data,
    common_name = common_name,
    return_positive_sites_only = return_positive_sites_only,
    verbose = verbose
  )
  bio_data <- format_hkl_bio_data(
    data = catch_data,
    common_name = common_name,
    verbose = verbose
  )
  hkl_data <- list()
  hkl_data$catch_data <- catch_data
  hkl_data$bio_data <- bio_data
  save_rdata(
    x = hkl_data,
    dir = dir,
    name_base = "nwfsc_hkl_catch_and_bio_data",
    verbose = verbose
  )
  return(hkl_data)
}
