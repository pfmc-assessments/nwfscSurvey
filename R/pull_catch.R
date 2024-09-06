#' Pull catch data for satisfactory tows from the NWFSC data warehouse
#'
#' Pull catch data from the
#' [NWFSC data warehouse](https://www.webapps.nwfsc.noaa.gov/data)
#' for a single species or all observed species, where the latter is specified
#' by leaving both `common_name = NULL` and `sci_name = NULL`.
#'
#' @details
#' The data available in the warehouse are cleaned prior to being downloaded
#' with the intent that they provide the best available information for use
#' in an index-standardization procedure. The removed samples may be of use
#' to others with a less-restrictive goal than producing an index of abundance.
#' For example, depths sampled outside the standard protocol are excluded.
#' To download all data, we currently recommend going to the
#' [NWFSC data warehouse](https://www.webapps.nwfsc.noaa.gov/data)
#' and using the csv link to extract data for a single species at a time.
#' In the future, we hope to add functionality to this package such that
#' downloading all data can be done easily within this function.
#'
#' @template common_name
#' @template sci_name
#' @template years
#' @template survey
#' @template dir
#' @template convert
#' @template verbose
#' @param standard_filtering A logical TRUE/FALSE that specifies whether data
#'   should be filtered using the standard filtering which removes tows with bad
#'   performance (water haul or poor net performance), or stations that have been
#'   removed from the survey sampling protocol.

#'
#' @author Chantel Wetzel
#' @export
#'
#' @import chron
#' @import cli
#' @importFrom stringr str_replace_all
#' @importFrom dplyr left_join rename
#'
#' @examples
#' \dontrun{
#' # survey is only arg that has to be specified
#' catch_data <- pull_catch(survey = "NWFSC.Combo")
#'
#' # Example with specified common name
#' catch_data <- pull_catch(
#'   common_name = "vermilion rockfish",
#'   survey = "NWFSC.Combo"
#' )
#'
#' # Example with specified scientific name
#' catch_data <- pull_catch(
#'   sci_name = "Eopsetta jordani",
#'   survey = "NWFSC.Combo"
#' )
#'
#' # Example with multiple names
#' catch_data <- pull_catch(common_name = c(
#'   "vermilion rockfish",
#'   "vermilion and sunset rockfish"
#' ), survey = "NWFSC.Combo")
#'
#' catch_data <- pull_catch(
#'   sci_name = c(
#'     "Sebastes miniatus",
#'     "Sebastes sp. (crocotulus)",
#'     "Sebastes sp. (miniatus / crocotulus)"
#'   ),
#'   survey = "NWFSC.Combo"
#' )
#' }
#'
pull_catch <- function(
    survey,
    common_name = NULL,
    sci_name = NULL,
    years = c(1970, 2050),
    dir = NULL,
    convert = TRUE,
    verbose = TRUE,
    sample_types = c("NA", NA, "Life Stage", "Size")[1:2],
    standard_filtering = TRUE) {
  if (survey %in% c("NWFSC.Shelf.Rockfish", "NWFSC.Hook.Line")) {
    cli::cli_abort(
      "The catch pull currently does not work for NWFSC Hook & Line Survey data.",
      "A subset of the data is available on the data warehouse https://www.webapp.nwfsc.noaa.gov/data",
      "Contact John Harms (john.harms@noaa.gov) for the full data set."
    )
  }

  if (length(c(common_name, sci_name)) != max(c(length(common_name), length(sci_name)))) {
    cli::cli_abort(
      "Function is unable to pull data using both the common_name or sci_name together.
      Please retry using only one."
    )
  }

  check_dir(dir = dir, verbose = verbose)

  if (is.null(common_name)) {
    var_name <- "scientific_name"
    species <- sci_name
  } else {
    var_name <- "common_name"
    species <- common_name
  }
  if (is.null(sci_name) & is.null(common_name)) {
    var_name <- "common_name"
    species <- "pull all"
  }

  # Survey options available in the data warehouse
  project_long <- check_survey(survey = survey)

  if (length(years) == 1) {
    years <- c(years, years)
  }

  # Pull data for the specific species for the following variables
  # Can only pull the nested fields (legacy performance and statistical partition) if
  # the main table fields are specified. Could pull separate and then join which
  # would allow us to eliminate vars_long form the main pull
  vars_long <- c(
    "common_name", "scientific_name", "project", "year", "vessel", "tow",
    "total_catch_numbers", "total_catch_wt_kg", "trawl_id",
    "subsample_count", "subsample_wt_kg", "cpue_kg_per_ha_der",
    "statistical_partition_dim$statistical_partition_type",
    "partition", "operation_dim$legacy_performance_code",
    "performance", "station_invalid", "depth_m"
  )

  # These are the retained and returned fields
  #vars_short <- vars_long[!vars_long %in% perf_codes]

  species_str <- convert_to_hex_string(species)
  add_species <- paste0("field_identified_taxonomy_dim$", var_name, "|=[", species_str, "]")

  if (any(species == "pull all")) {
    add_species <- ""
  }

  url_text <- get_url(
    data_table = "trawl.catch_fact",
    project_long = project_long,
    add_species = add_species,
    years = years,
    vars_long = vars_long
  )

  if (verbose) {
    cli::cli_alert_info(
      "Pulling catch data for {species}. This can take up to ~ 30 seconds (or more)."
    )
  }

  # Pull data from positive tows for selected species
  positive_tows <- try(get_json(url = url_text))
  if (!is.data.frame(positive_tows)) {
    cli::cli_abort(
      "There are no tows where {species} was caught."
    )
  }

  # Now start filtering out tows that have issues:
  good_performance <- which(positive_tows$performance == "Satisfactory")
  if (length(good_performance) != dim(positive_tows)[1]) {
    if (verbose) {
      n <- length(which(positive_tows$performance != "Satisfactory" & !is.na(positive_tows$total_catch_numbers)))
      cli::cli_alert_info(
        "There were {n} positive tows with non-satisfactory tow performance (e.g., no area swept estimate, net issues, etc.)."
      )
    }
    if (standard_filtering) {
      positive_tows <- positive_tows[good_performance, ]
    }
  }

  good_station <- which(positive_tows$station_invalid == 0)
  if (length(good_station) != dim(positive_tows)[1]) {
    if (verbose) {
      n <- sum(!is.na(positive_tows[-good_station, "total_catch_numbers"]))
      cli::cli_alert_info(
        "There were {n} positive tows from stations that have been removed from the standard station list."
      )
    }
    if (standard_filtering) {
      positive_tows <- positive_tows[good_station, ]
    } else {
      positive_tows[good_station, "station_invalid"] <- "good_station"
    }
  }

  # Non-NA entries are only present in older surveys (e.g., Triennial) so this fills
  # in a default value for later surveys to keep then
  na_legacy_code <- is.na(positive_tows[, "operation_dim$legacy_performance_code"])
  if (sum(na_legacy_code) > 0) {
    positive_tows[na_legacy_code, "operation_dim$legacy_performance_code"] <- -999
  }
  water_hauls <- which(positive_tows[, "operation_dim$legacy_performance_code"] == 8)
  if (length(water_hauls) > 0) {
    if (verbose) {
      n <- length(water_hauls)
      cli::cli_alert_info(
        "There were {n} tows that were determined to be water hauls (net not on the bottom)."
      )
    }
    if (standard_filtering) {
      positive_tows <- positive_tows[-water_hauls, ]
    } else {
      positive_tows[water_hauls, "operation_dim$legacy_performance_code"] <- "water_hauls"
    }
  }

  bad_sample_types <- which(!positive_tows[, "statistical_partition_dim$statistical_partition_type"] %in% sample_types)
  if (length(bad_sample_types) > 0) {
    if (verbose) {
      cli::cli_alert_info(
        "There were {length(bad_sample_types)} positive tows where the sample type was not requested."
      )
    }
    positive_tows <- positive_tows[-bad_sample_types,]
  }

  # Remove tows outside of standard depths 55-1,280 m
  good_depth <- which(positive_tows$depth_m >= 55 & positive_tows$depth_m <= 1280)
  if (length(good_depth) != dim(positive_tows)[1]) {
    if (verbose) {
      n <- dim(positive_tows)[1] - length(good_depth)
      cli::cli_alert_info(
        "There were {n} tows that are outside the standard depth range."
      )
    }
    if (standard_filtering) {
      positive_tows <- positive_tows[good_depth, ]
    }
  }


  if (sum(is.na(positive_tows[, "common_name"])) > 0) {
    replace <- which(is.na(positive_tows[, "common_name"]))
    positive_tows[replace, "common_name"] <- positive_tows[replace, "scientific_name"]
  }

  positive_tows <- positive_tows[, colnames(positive_tows) != "depth_m"]

  # Pull all tow data including tows where the species was not observed
  vars_long <- c(
    "project", "year", "vessel", "pass", "tow", "datetime_utc_iso",
    "depth_hi_prec_m", "longitude_dd", "latitude_dd", "area_swept_ha_der",
    "trawl_id", "operation_dim$legacy_performance_code", "performance", "station_invalid"
  )

  url_text <- get_url(
    data_table = "trawl.operation_haul_fact",
    project_long = project_long,
    years = years,
    vars_long = vars_long
  )

  all_tows <- try(get_json(url = url_text))

  all_tows[, "depth_m"] <- all_tows[, "depth_hi_prec_m"]

  # Now start filtering out tows that have issues:
  good_performance <- which(all_tows$performance == "Satisfactory")
  if (length(good_performance) != dim(all_tows)[1]) {
    if (standard_filtering) {
      all_tows <- all_tows[good_performance, ]
    }
  }

  good_station <- which(all_tows$station_invalid == 0)
  if (length(good_station) != dim(all_tows)[1]) {
    if (standard_filtering) {
      all_tows <- all_tows[good_station, ]
    } else {
      all_tows[-good_station, "station_invalid"] <- "non_standard_station"
    }
  }

  # Non-NA entries are only present in older surveys (e.g., Triennial) so this fills
  # in a default value for later surveys to keep then
  na_legacy_code <- is.na(all_tows[, "operation_dim$legacy_performance_code"])
  if (sum(na_legacy_code) > 0) {
    all_tows[na_legacy_code, "operation_dim$legacy_performance_code"] <- -999
  }
  water_hauls <- which(all_tows[, "operation_dim$legacy_performance_code"] == 8)
  if (length(water_hauls) > 0) {
    if (standard_filtering) {
      all_tows <- all_tows[-water_hauls, ]
    } else {
      all_tows[water_hauls, "operation_dim$legacy_performance_code"] <- "water_hauls"
    }
  }

  # Remove tows outside of standard depths 55-1,280 m
  good_depth <- which(all_tows$depth_m >= 55 & all_tows$depth_m <= 1280)
  if (length(good_depth) != dim(all_tows)[1]) {
    if (standard_filtering) {
      all_tows <- all_tows[good_depth, ]
    }
  }

  all_tows <- all_tows[
    !duplicated(paste(all_tows$year, all_tows$pass, all_tows$vessel, all_tows$tow)),
  ]

  positive_tows_grouped <- dplyr::group_by(
    .data = positive_tows,
    common_name, scientific_name
  )
  # Split positive_tows into 1 data frame for each combination of common_name
  # and scientific_name and store in a named list for purrr::map()
  positive_tows_split <- dplyr::group_split(positive_tows_grouped)
  group_names <- dplyr::group_keys(positive_tows_grouped)
  names(positive_tows_split) <- tidyr::unite(group_names, col = "groups") |>
    dplyr::pull(groups)

  # For each data frame in the large list, find the tows that are not present
  # in positive_tows and join them into a single data frame
  # Give them the appropriate common and scientific names using .id then split
  # the concatenated column out into the two original columns
  names_intersect <- intersect(colnames(all_tows), colnames(positive_tows))
  zero_tows <- purrr::map_df(
    .x = positive_tows_split,
    .f = \(y) dplyr::anti_join(x = all_tows, y = y, by = names_intersect),
    .id = "groups"
  ) |>
    tidyr::separate_wider_delim(
      cols = "groups",
      delim = "_",
      names = colnames(group_names)
    )

  # Join the positive tows with the tow information
  positive_tows_with_tow_info <- dplyr::left_join(
    x = positive_tows,
    y = all_tows,
    by = intersect(colnames(all_tows), colnames(positive_tows))
  )
  # Join the augmented positive tow information with the zero tows
  # arrange by common_name and tow_id
  catch <- dplyr::full_join(
    x = positive_tows_with_tow_info,
    y = zero_tows,
    by = c(colnames(group_names), colnames(all_tows))
  ) |>
    dplyr::arrange(common_name, trawl_id)

  colnames(catch)[colnames(catch) == "statistical_partition_dim$statistical_partition_type"] <- "partition_sample_types"
  colnames(catch)[colnames(catch) == "operation_dim$legacy_performance_code"] <- "legacy_performance_code"

  no_area <- which(is.na(catch$area_swept_ha_der))
  if (length(no_area) > 0) {
    if (verbose) {
      n <- length(no_area)
      cli::cli_alert_info(
        "There were {n} tows with no area swept calculation and will be filled with the mean swept area across all tows."
      )
    }
    if (standard_filtering) {
      catch[no_area, "area_swept_ha_der"] <- mean(catch$area_swept_ha_der, trim = 0.05, na.rm = TRUE)
    }
  }

  # Fill in zeros where needed
  catch[is.na(catch)] <- 0
  catch[catch[, "partition_sample_types"] == 0, "partition_sample_types"] <- NA
  catch[catch[, "partition"] == 0, "partition"] <- NA

  catch$date <- chron::chron(
    format(as.POSIXlt(catch$datetime_utc_iso, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"),
    format = "y-m-d", out.format = "YYYY-m-d"
  )

  catch$trawl_id <- as.character(catch$trawl_id)
  # kg / km2 <- (100 hectare / 1 *km2) * (kg / hectare)
  catch$cpue_kg_km2 <- catch$cpue_kg_per_ha_der * 100
  colnames(catch)[which(colnames(catch) == "area_swept_ha_der")] <- "area_swept_ha"

  find <- grep("trawl_id", colnames(catch), ignore.case = TRUE)
  n_id <- table(catch[, find])
  if (any(n_id != 1)) {
    if (!"pull all" %in% species) {
      if (verbose) {
        cli::cli_alert_warning(
          "There are multiple records for unique tows (trawl_id). This could be
          because all sample types were included, multiple records for cryptic
          species pairs were returned, or multiple species were requested.
          The `combine_tows` function can be used to combine these multiple records
          for unique tows if needed."
        )
      }
    }
  }

  if (convert) {
    firstup <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    colnames(catch) <- firstup(colnames(catch))
    colnames(catch)[colnames(catch) == "Cpue_kg_km2"] <- "cpue_kg_km2"
    colnames(catch)[colnames(catch) == "Cpue_kg_per_ha_der"] <- "cpue_kg_per_ha_der"
    colnames(catch)[colnames(catch) == "Total_catch_numbers"] <- "total_catch_numbers"
    colnames(catch)[colnames(catch) == "Total_catch_wt_kg"] <- "total_catch_wt_kg"
  }

  save_rdata(
    x = catch,
    dir = dir,
    name_base = paste0("catch_", species, "_", survey),
    verbose = verbose
  )

  return(catch)
}
