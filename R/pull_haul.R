#' Pull haul data from the NWFSC data warehouse.
#' The website is: https://www.webapps.nwfsc.noaa.gov/data.
#' This function can be used to pull haul data and associated covariates.
#'
#' @template years 
#' @template survey 
#' @template dir 
#' @template verbose 
#'
#' @return Returns a data frame of haul characteristics for satisfactory hauls
#' @author Eric Ward, Chantel Wetzel
#' @export
#'
#' @import jsonlite
#' @import glue
#'
#' @examples
#' \dontrun{
#' haul_data <- pull_haul(survey = "NWFSC.Combo", years = c(2003, 2007))
#' haul_data <- pull_haul()
#' }
#'
pull_haul <- function(years= c(1980, 2050), survey = NULL, dir = NULL, verbose = TRUE) {

  # increase the timeout period to avoid errors when pulling data
  options(timeout = 4000000)

  check_dir(dir = dir, verbose = verbose)  

  project_long <- check_survey(survey = survey)

  if (length(years) == 1) {
    years <- c(years, years)
  }

  var_str <- c(
    "area_swept_ha_der", "date_dim.year", "date_yyyymmdd",
    "depth_hi_prec_m", "door_width_m_der", "fluorescence_at_surface_mg_per_m3_der",
    "gear_end_latitude_dd", "gear_end_longitude_dd", "gear_start_latitude_dd",
    "gear_start_longitude_dd", "invertebrate_weight_kg", "latitude_dd", "leg",
    "longitude_dd", "net_height_m_der", "net_width_m_der", "nonspecific_organics_weight_kg",
    "o2_at_gear_ml_per_l_der", "pass", "performance", "project", "salinity_at_gear_psu_der",
    "sampling_end_hhmmss", "sampling_start_hhmmss",
    "target_station_design_dim.stn_invalid_for_trawl_date_whid",
    "temperature_at_gear_c_der", "temperature_at_surface_c_der",
    "trawl_id", "turbidity_ntu_der", "vertebrate_weight_kg", "vessel",
    "vessel_end_latitude_dd", "vessel_end_longitude_dd",
    "vessel_start_latitude_dd", "vessel_start_longitude_dd"
  )

  url_text <- get_url(data_table = "trawl.operation_haul_fact", 
                      years = years, 
                      project_long = project_long,
                      vars_long = var_str) 


  if (verbose) {
    message("Pulling haul data. This can take up to ~ 30 seconds.")
  }
  haul_data <- try(jsonlite::fromJSON(url_text))

  haul_data$date_formatted <- 
    chron::chron(format(as.POSIXlt(haul_data$date, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), 
    format = "y-m-d", out.format = "YYYY-m-d")
  
  if (is.null(dir)) {
    time <- substring(Sys.time(), 1, 10)
    save(haul_data, file = file.path(dir, paste("haul_", survey, "_", time, ".rda", sep = "")))
    if (verbose) {
      message(
        glue::glue("Haul data file saved to following location: {dir}"))
    }
  }

  return(haul_data)
}
