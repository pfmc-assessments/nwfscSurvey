#' Filter data pulls
#'
#' Function to create messages on data that are outside the standard survey protocol
#' and to remove these samples if `standard_filtering` = TRUE. The data are checked
#' for tow performance, valid stations, and depth range. This function is called
#' within the pull functions, but can be called on pulled data frames if filtering
#' was not selected in the original data pull.
#'
#' @param data Data frame of pulled data created by the [pull_catch()], [pull_bio()],
#'   [pull_haul()], or [pull_biological_samples()].
#' @param data_type Character string to include within data filtering messages
#'   to indicate the type of data being filtered such as "biological samples".
#'   Default is "samples".
#' @param standard_filtering A logical TRUE/FALSE that specifies whether data
#'   should be filtered using the standard filtering which removes tows with bad
#'   performance (water haul or poor net performance), or stations that have been
#'   removed from the survey sampling protocol.
#' @param verbose A logical that specifies if you want to print messages and
#'   warnings to the console. The default is `TRUE`.
#'
#' @author Chantel Wetzel
#' @family helper function
#' @export
#'
#' @import cli
#'
filter_pull <- function(
  data,
  data_type = "samples",
  standard_filtering = TRUE,
  verbose = TRUE
) {
  # Now start filtering out tows that have issues:
  good_performance <- which(data$performance == "Satisfactory")
  if (length(good_performance) != dim(data)[1]) {
    if (verbose) {
      if ("cpue_kg_km2" %in% colnames(data)) {
        n <- length(which(
          data$performance != "Satisfactory" & data$cpue_kg_km2 > 0
        ))
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

  good_station <- which(
    data$is_actual_station_currently_active == TRUE
  )
  if (
    any(
      data$project ==
        "West Coast Groundfish Bottom Trawl Slope/Shelf Combination Survey"
    )
  ) {
    if (data_type == "tows") {
      n_positive <- dim(data)[1] - length(good_station)
    } else {
      n_positive <- sum(
        data[-good_station, "total_catch_numbers"] > 0,
        na.rm = TRUE
      )
    }
    if (any(c("net_height_m_der", "length_cm") %in% colnames(data))) {
      n <- dim(data)[1] - length(good_station)
    }
    if (verbose) {
      cli::cli_alert_info(
        "There are {n_positive} {data_type} from stations that are no long active survey stations that are retained in the data.
      Prior to June 2026, data from these stations were removed when standard_filtering = TRUE.
      These tows can be identified using the is_actual_station_currently_active column."
      )
    }
  }

  # Remove tows outside of standard depths 55-1,280 m
  if (any(c("depth_m") %in% colnames(data))) {
    if (sum(is.na(data[, "depth_m"])) == nrow(data)) {
      if (verbose) {
        cli::cli_alert_info(
          "All depth_m values were NA. No records were removed but data should be investigated."
        )
      }
    } else {
      col_to_use <- which(colnames(data) %in% "depth_m")
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
    }
  }

  return(data)
}
