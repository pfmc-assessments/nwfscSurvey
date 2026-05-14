#' Pull haul data from pinned data list on posit connect
#'
#' This function can be used to return pinned data for trawl haul information.
#'
#' @inheritParams pull_catch
#'
#' @return Returns a data frame of haul characteristics for satisfactory hauls
#' @author Chantel Wetzel
#' @family data pulling functions
#' @export
#'
#' @import cli
#'
pull_haul_cache <- function(
  survey = "NWFSC.Combo",
  years = c(1970, 2050),
  dir = NULL,
  verbose = TRUE,
  standard_filtering = TRUE
) {
  data_list <- pins::pin_read(
    pins::board_connect(),
    "chantel.wetzel/nwfsc_trawl_survey_haul_data"
  )
  if (survey == "Triennial") {
    data <- data_list$triennial
  }
  if (survey == "AFSC.Slope") {
    data <- data_list$afsc_slope
  }
  if (survey == "NWFSC.Slope") {
    data <- data_list$nwfsc_slope
  }
  if (survey == "NWFSC.Combo") {
    data <- data_list$wcgbt
  }
  year_range <- years[1]:years[2]
  haul_data <- data |>
    dplyr::filter(year %in% year_range)
  haul_data <- filter_pull(
    data = haul_data,
    data_type = "positive tows",
    standard_filtering = standard_filtering,
    verbose = verbose
  )
  save_rdata(
    x = haul_data,
    dir = dir,
    name_base = paste0("haul_", survey),
    verbose = verbose
  )
  return(haul_data)
}
