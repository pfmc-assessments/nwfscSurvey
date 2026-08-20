#' Utility function used throughout the package
#'
#' @details
#' Function that converts a string to a hex string
#' for common name or scientific name when pulling
#' data. This function is used within the pull_*
#' functions that retrieve species specific data
#'
#' @param x A string of either common_name or
#' sci_name
#' @author Kelli Johnson
#' @export
#'
#' @examples
#' \dontrun{
#' common_name <- c("lingcod", "sablefish", "Pacific cod")
#' convert_to_hex_string(common_name)
#' }
#'
convert_to_hex_string <- function(x) {
  eq_symbol <- "eq:"
  hex_space <- "+"
  stopifnot(inherits(x, "character"))
  x_no_spaces <- firstup(gsub(pattern = " ", replacement = hex_space, x))
  if (length(x_no_spaces) > 1) {
    x_no_spaces <- paste(x_no_spaces, collapse = "~")
    eq_symbol <- "in:"
  }
  out <- paste0(eq_symbol, x_no_spaces)
  return(out)
}
#' Utility function used to convert column names
#'
#' @details
#' Function that converts the new data warehouse
#' column names to the previous column names
#' from the trips/api
#'
#' @param x A data frame with named columns
#' @return A data frame with renamed columns
#' @author Chantel Wetzel
#'
#'
convert_colnames <- function(x) {
  converted_df <- matrix(
    scan(
      text = "
          best_available_taxon_common_name  common_name
          best_available_taxon_scientific_name  scientific_name
          nmfs_project_name  project
          survey_year  year
          vessel_name  vessel
          pass_number  pass
          tow_sequence_number tow
          sampling_date  date
          total_catch_individuals_count total_catch_numbers
          total_catch_weight_kg  total_catch_wt_kg
          bottom_trawl_operation_key  trawl_id
          sampled_catch_individuals_count  subsample_count
          sampled_catch_weight_kg  subsample_wt_kg
          catch_per_unit_effort_kg_per_ha  cpue_kg_per_ha_der
          tow_performance_name  performance
          actual_station_current_deactivation_reasons  actual_station_design_dim$reason_station_invalid
          is_actual_station_currently_active  station_invalid
          on_bottom_seafloor_depth_m  depth_m
          seafloor_area_swept_ha area_swept_ha_der
          tow_sea_surface_fluorescence_mg_per_m3 fluorescence_at_surface_mg_per_m3_der
          net_off_bottom_gear_latitude_dd  gear_end_latitude_dd
          net_off_bottom_gear_longitude_dd gear_end_longitude_dd
          net_on_bottom_gear_latitude_dd gear_start_latitude_dd
          net_on_bottom_gear_longitude_dd gear_start_longitude_dd
          net_off_bottom_vessel_latitude_dd vessel_end_latitude_dd
          net_off_bottom_vessel_longitude_dd vessel_end_longitude_dd
          net_on_bottom_vessel_latitude_dd vessel_start_latitude_dd
          net_on_bottom_vessel_longitude_dd vessel_start_longitude_dd
          best_tow_latitude_dd latitude_dd
          best_tow_longitude_dd longitude_dd
          invertebrate_catch_weight_kg invertebrate_weight_kg
          on_bottom_net_door_spread_m door_width_m_der
          on_bottom_net_headrope_height_m  net_height_m_der
          on_bottom_net_wing_spread_m net_width_m_der
          nonspecific_organics_catch_weight_kg nonspecific_organics_weight_kg
          on_bottom_dissolved_oxygen_ml_per_l o2_at_gear_ml_per_l_der
          on_bottom_salinity_psu salinity_at_gear_psu_der
          started_haulback_at  started_haulback_at
          net_on_bottom_at  net_on_bottom_at
          on_bottom_water_temperature_c temperature_at_gear_c_der
          tow_sea_surface_water_temperature_c temperature_at_surface_c_der
          on_bottom_water_turbidity_ntu turbidity_ntu_der
          vertebrate_catch_weight_kg vertebrate_weight_kg
          actual_station_deactivation_year target_station_design_dim.stn_invalid_for_trawl_date_whid
          actual_station_current_deactivation_reasons actual_station_design_dim$reason_station_invalid
          life_stage_name partition_sample_types
          specimen_weight_kg weight_kg
          ageing_lab_name ageing_lab
          specimen_age_sample_label otosag_id
          specimen_size_cm length_cm
          specimen_sex_code sex
          specimen_age_years age_years
      ",
      quiet = TRUE,
      what = "",
      strip.white = TRUE
    ),
    ncol = 2,
    byrow = TRUE
  )
  colnames(converted_df) <- c("new_names", "old_names")
  matches <- match(colnames(x), converted_df[, "new_names"])
  colnames(x) <- ifelse(
    is.na(matches),
    colnames(x),
    converted_df[matches, "old_names"]
  )
  return(x)
}
#' Capitalize first letter in a string
#'
#' @details
#' Function that converts the first letter in a
#' string to a capital letter
#'
#' @param x A data frame with named columns
#' @return A data frame with renamed columns
#' @author Chantel Wetzel
#'
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
