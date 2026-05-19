#' Pull nwfsc hkl data from pinned data list on posit connect
#'
#' This function can be used to return pinned data for NWFSC Hook-and-Line Survey.
#'
#' @inheritParams pull_catch
#'
#' @return Returns a data frame of all hkl data
#' @author Chantel Wetzel
#' @family data pulling functions
#' @export
#'
#' @import cli
#'
pull_hkl_cache <- function(
  years = c(1970, 2050),
  dir = NULL,
  verbose = TRUE
) {
  board <- pins::board_url(
    c(
      dataset = "https://connect.fisheries.noaa.gov/nwfsc_hkl_survey_data/"
    )
  )
  data <- pins::pin_read(
    board,
    "dataset"
  )
  year_range <- years[1]:years[2]
  hkl_data <- data |>
    dplyr::filter(year %in% year_range)
  save_rdata(
    x = hkl_data,
    dir = dir,
    name_base = "nwfsc_hkl_data",
    verbose = verbose
  )
  return(hkl_data)
}
