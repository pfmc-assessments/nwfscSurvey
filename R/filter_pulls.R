#' Filter data pulls
#'
#' Function to create messages on data that are outside the standard survey protocol
#' and to remove these samples if `standard_filtering` = TRUE. The data are checked
#' for tow performance, valid stations, and depth range. This function is called
#' within the pull functions, but can be called on pulled data frames if filtering
#' was not selected in the original data pull.
#'
#' @param data Data frame of pulled data created by the [pull_catch()], [pull_bio()],
#'   [pull_haul()], or [pull_biological_samples].
#' @param data_type Character string to include within data filtering messages
#'   to indicate the type of data being filtered.
#' @param standard_filtering A logical TRUE/FALSE that specifies whether data
#'   should be filtered using the standard filtering which removes tows with bad
#'   performance (water haul or poor net performance), or stations that have been
#'   removed from the survey sampling protocol.
#' @template verbose
#'
#' @author Chantel Wetzel
#' @export
#'
#' @import cli
#'
filter_pull <- function(
    data,
    data_type,
    standard_filtering = TRUE,
    verbose = TRUE) {
  # Now start filtering out tows that have issues:
  good_performance <- which(data$performance == "Satisfactory")
  if (length(good_performance) != dim(data)[1]) {
    if (verbose) {
      if ("total_catch_numbers" %in% colnames(data)) {
        n <- length(which(data$performance != "Satisfactory" & data$total_catch_numbers > 0))
      } else {
        n <- length(which(data$performance != "Satisfactory"))
      }
      cli::cli_alert_info(
        "There were {n} {data_type} with non-satisfactory tow performance (e.g., no area swept estimate, net issues, etc.)."
      )
    }
    if (standard_filtering) {
      data <- data[good_performance, ]
    }
  }

  good_station <- which(data$station_invalid == 0)
  if (length(good_station) != dim(data)[1]) {
    if (verbose) {
      n <- sum(!is.na(data[-good_station, "total_catch_numbers"]))
      if (any(c("net_height_m_der", "length_cm") %in% colnames(data))) {
        n <- dim(data)[1] - length(good_station)
      }
      cli::cli_alert_info(
        "There were {n} {data_type} from stations that have been removed from the standard station list."
      )
    }
    if (standard_filtering) {
      data <- data[good_station, ]
    } else {
      data[good_station, "station_invalid"] <- "good_station"
    }
  }

  # Non-NA entries are only present in older surveys (e.g., Triennial) so this fills
  # in a default value for later surveys to keep then
  na_legacy_code <- is.na(data[, "operation_dim$legacy_performance_code"])
  if (sum(na_legacy_code) > 0) {
    data[na_legacy_code, "operation_dim$legacy_performance_code"] <- -999
  }
  water_hauls <- which(data[, "operation_dim$legacy_performance_code"] == 8)
  if (length(water_hauls) > 0) {
    if (verbose) {
      n <- length(water_hauls)
      cli::cli_alert_info(
        "There were {n} {data_type} that were determined to be water hauls (net not on the bottom)."
      )
    }
    if (standard_filtering) {
      data <- data[-water_hauls, ]
    } else {
      data[water_hauls, "operation_dim$legacy_performance_code"] <- "water_hauls"
    }
  }

  # Remove tows outside of standard depths 55-1,280 m
  col_to_use <- which(colnames(data) %in% c("depth_hi_prec_m", "depth_m"))
  good_depth <- which(data[, col_to_use] >= 55 & data[, col_to_use] <= 1280)
  if (length(good_depth) != dim(data)[1]) {
    if (verbose) {
      n <- dim(data)[1] - length(good_depth)
      cli::cli_alert_info(
        "There were {n} {data_type} that are outside the standard depth range."
      )
    }
    if (standard_filtering) {
      data <- data[good_depth, ]
    }
  }

  return(data)

}
