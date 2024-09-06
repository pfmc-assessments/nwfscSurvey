#' Pull haul data from the NWFSC data warehouse.
#'
#' The website is: https://www.webapps.nwfsc.noaa.gov/data.
#' This function can be used to pull haul data and associated covariates.
#'
#' @template years
#' @template survey
#' @template dir
#' @template verbose
#' @param standard_filtering A logical TRUE/FALSE that specifies whether data
#'   should be filtered using the standard filtering which removes tows with bad
#'   performance (water haul or poor net performance), or stations that have been
#'   removed from the survey sampling protocol.
#'
#' @return Returns a data frame of haul characteristics for satisfactory hauls
#' @author Eric Ward, Chantel Wetzel
#' @export
#'
#' @import cli
#'
#' @examples
#' \dontrun{
#' haul_data <- pull_haul(survey = "NWFSC.Combo", years = c(2003, 2007))
#' haul_data <- pull_haul()
#' }
#'
pull_haul <- function(
    survey,
    years = c(1970, 2050),
    dir = NULL,
    verbose = TRUE,
    standard_filtering = TRUE) {
  # increase the timeout period to avoid errors when pulling data
  options(timeout = 4000000)

  check_dir(dir = dir, verbose = verbose)

  project_long <- check_survey(survey = survey)

  if (length(years) == 1) {
    years <- c(years, years)
  }

  var_str <- c(
    "area_swept_ha_der", "year", "datetime_utc_iso",
    "depth_hi_prec_m", "door_width_m_der", "fluorescence_at_surface_mg_per_m3_der",
    "gear_end_latitude_dd", "gear_end_longitude_dd", "gear_start_latitude_dd",
    "gear_start_longitude_dd", "invertebrate_weight_kg", "latitude_dd", "leg",
    "longitude_dd", "net_height_m_der", "net_width_m_der", "nonspecific_organics_weight_kg",
    "o2_at_gear_ml_per_l_der", "pass", "performance", "project", "salinity_at_gear_psu_der",
    "station_invalid", "sampling_end_hhmmss", "sampling_start_hhmmss",
    "target_station_design_dim.stn_invalid_for_trawl_date_whid",
    "temperature_at_gear_c_der", "temperature_at_surface_c_der",
    "trawl_id", "turbidity_ntu_der", "vertebrate_weight_kg", "vessel",
    "vessel_end_latitude_dd", "vessel_end_longitude_dd",
    "vessel_start_latitude_dd", "vessel_start_longitude_dd"
  )

  url_text <- get_url(
    data_table = "trawl.operation_haul_fact",
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

  haul_data$date_formatted <-
    chron::chron(format(as.POSIXlt(haul_data$datetime_utc_iso, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"),
      format = "y-m-d", out.format = "YYYY-m-d"
    )

  haul_data$trawl_id <- as.character(haul_data$trawl_id)

  # Now start filtering out tows that have issues:
  good_performance <- which(haul_data$performance == "Satisfactory")
  if (length(good_performance) != dim(haul_data)[1]) {
    if (verbose) {
      n <- length(which(haul_data$performance != "Satisfactory"))
      cli::cli_alert_info(
        "There were {n} tows with non-satisfactory tow performance (e.g., no area swept estimate, net issues, etc.)."
      )
    }
    if (standard_filtering) {
      haul_data <- haul_data[good_performance, ]
    }
  }

  good_station <- which(haul_data$station_invalid == 0)
  if (length(good_station) != dim(haul_data)[1]) {
    if (verbose) {
      n <- length(which(haul_data$station_invalid != 0))
      cli::cli_alert_info(
        "There were {n} tows from stations that have been removed from the standard station list."
      )
    }
    if (standard_filtering) {
      haul_data <- haul_data[good_station, ]
    } else {
      haul_data[good_station, "station_invalid"] <- "good_station"
    }
  }

  # Remove tows outside of standard depths 55-1,280 m
  good_depth <- which(haul_data$depth_hi_prec_m >= 55 & haul_data$depth_hi_prec_m <= 1280)
  if (length(good_depth) != dim(haul_data)[1]) {
    if (verbose) {
      n <- dim(haul_data)[1] - length(good_depth)
      cli::cli_alert_info(
        "There were {n} tows that are outside the standard depth range."
      )
    }
    if (standard_filtering) {
      haul_data <- haul_data[good_depth, ]
    }
  }

  save_rdata(
    x = haul_data,
    dir = dir,
    name_base = paste0("haul_", survey),
    verbose = verbose
  )

  return(haul_data)
}
