#' Pull catch data from pinned data list on posit connect
#'
#' This function can be used to return pinned data for trawl catch data.
#'
#' @inheritParams pull_catch
#'
#' @return Returns a data frame of catch data
#' @author Chantel Wetzel
#' @family data pulling functions
#' @export
#'
#' @import cli
#'
pull_catch_cache <- function(
  common_name = NULL,
  sci_name = NULL,
  survey = "NWFSC.Combo",
  years = c(1970, 2050),
  dir = NULL,
  convert = TRUE,
  verbose = TRUE,
  standard_filtering = TRUE
) {
  if (is.null(common_name)) {
    var_name <- "scientific_name"
    species <- sci_name
  } else {
    var_name <- "common_name"
    species <- common_name
  }
  if (is.null(sci_name) & is.null(common_name)) {
    var_name <- c("scientific_name", "common_name")
    species <- "pull all"
  }
  #data_list <- pins::pin_read(
  #  pins::board_connect(),
  #  "chantel.wetzel/nwfsc_trawl_survey_catch_data"
  #)
  board <- pins::board_url(
    c(
      dataset = "https://connect.fisheries.noaa.gov/nwfsc_trawl_survey_catch_data/"
    )
  )
  data_list <- pins::pin_read(
    board,
    "dataset"
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
  data_tolower <- data |>
    dplyr::rename_with(tolower)
  year_range <- years[1]:years[2]
  data_years <- data_tolower |>
    dplyr::filter(year %in% year_range)
  if (!is.null(common_name)) {
    data_species <- data_years |>
      dplyr::filter(common_name %in% species)
  }
  if (!is.null(sci_name)) {
    data_species <- data_years |>
      dplyr::filter(scientific_name %in% species)
  }
  catch <- filter_pull(
    data = data_species,
    data_type = "positive tows",
    standard_filtering = standard_filtering,
    verbose = verbose
  )
  if (convert) {
    firstup <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    colnames(catch) <- firstup(colnames(catch))
    colnames(catch)[colnames(catch) == "Cpue_kg_km2"] <- "cpue_kg_km2"
    colnames(catch)[
      colnames(catch) == "Cpue_kg_per_ha_der"
    ] <- "cpue_kg_per_ha_der"
    colnames(catch)[
      colnames(catch) == "Total_catch_numbers"
    ] <- "total_catch_numbers"
    colnames(catch)[
      colnames(catch) == "Total_catch_wt_kg"
    ] <- "total_catch_wt_kg"
  }
  save_rdata(
    x = catch,
    dir = dir,
    name_base = paste0("catch_", species, "_", survey),
    verbose = verbose
  )
  return(catch)
}
