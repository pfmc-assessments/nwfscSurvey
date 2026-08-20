#' Pull haul data from the NWFSC data warehouse.
#'
#' This function can be used to pull haul data and associated covariates.
#'
#' @inheritParams pull_catch
#'
#' @return Returns a data frame of haul characteristics for satisfactory hauls
#' @author Eric Ward and Chantel Wetzel
#' @family data pulling functions
#' @export
#'
#' @import cli
#'
#' @examples
#' \dontrun{
#' haul_data <- pull_haul(years = c(2003, 2007))
#' haul_data <- pull_haul(survey = "Triennial", years = c(1980, 2004))
#' }
#'
pull_haul <- function(
  survey = "NWFSC.Combo",
  years = c(1980, 2050),
  dir = NULL,
  verbose = TRUE,
  standard_filtering = TRUE
) {
  # increase the timeout period to avoid errors when pulling data
  options(timeout = 4000000)

  check_dir(dir = dir, verbose = verbose)

  project_long <- check_survey(survey = survey)

  if (length(years) == 1) {
    years <- c(years, years)
  }

  var_str <- c(
    "seafloor_area_swept_ha",
    "survey_year",
    "sampling_date",
    "tow_sequence_number",
    "tow_sea_surface_fluorescence_mg_per_m3",
    "net_off_bottom_gear_latitude_dd",
    "net_off_bottom_gear_longitude_dd",
    "net_on_bottom_gear_latitude_dd",
    "net_on_bottom_gear_longitude_dd",
    "invertebrate_catch_weight_kg",
    "best_tow_longitude_dd",
    "best_tow_latitude_dd",
    "leg_number",
    "on_bottom_seafloor_depth_m",
    "on_bottom_net_door_spread_m",
    "on_bottom_net_headrope_height_m",
    "on_bottom_net_wing_spread_m",
    "nonspecific_organics_catch_weight_kg",
    "on_bottom_dissolved_oxygen_ml_per_l",
    "pass_number",
    "tow_performance_name",
    "nmfs_project_name",
    "on_bottom_salinity_psu",
    "is_actual_station_currently_active",
    "was_actual_station_active_when_sampled",
    "started_haulback_at",
    "net_on_bottom_at",
    "actual_station_deactivation_year",
    "on_bottom_water_temperature_c",
    "temperature_at_gear_c_der",
    "tow_sea_surface_water_temperature_c",
    "bottom_trawl_operation_key",
    "on_bottom_water_turbidity_ntu",
    "vertebrate_catch_weight_kg",
    "vessel_name",
    "net_off_bottom_vessel_latitude_dd",
    "net_off_bottom_vessel_longitude_dd",
    "net_on_bottom_vessel_latitude_dd",
    "net_on_bottom_vessel_longitude_dd",
    "actual_station_current_deactivation_reasons"
  )

  url_text <- get_url(
    data_table = "tows",
    years = years,
    project_long = project_long,
    vars_long = var_str
  )

  if (verbose) {
    cli::cli_alert_info(
      "Pulling haul data. This can take up to ~ 30 seconds (or more)."
    )
  }
  haul_data <- try(get_json(url = url_text))
  if (inherits(haul_data, "try-error")) {
    cli::cli_alert_danger(
      "The data request failed. The data warehouse may be offline. Please use pull_haul_cache() to access data."
    )
    cli::cli_abort("")
  }
  haul_data_convert <- convert_colnames(
    x = haul_data
  )
  haul_data_filtered <- filter_pull(
    data = haul_data_convert,
    data_type = "tows",
    standard_filtering = standard_filtering,
    verbose = verbose
  )

  colnames(haul_data_filtered)[
    colnames(haul_data_filtered) ==
      "actual_station_design_dim$reason_station_invalid"
  ] <- "reason_station_invalid"
  haul_data_filtered$trawl_id <- as.character(haul_data_filtered$trawl_id)

  save_rdata(
    x = haul_data_filtered,
    dir = dir,
    name_base = paste0("haul_", survey),
    verbose = verbose
  )

  return(haul_data_filtered)
}
