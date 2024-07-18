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
#' @template sample_types
#'
#' @author Chantel Wetzel
#' @export
#'
#' @import chron
#' @importFrom stringr str_replace_all
#' @importFrom dplyr left_join rename
#' @import glue
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
pull_catch <- function(common_name = NULL,
                       sci_name = NULL,
                       years = c(1970, 2050),
                       survey,
                       dir = NULL,
                       convert = TRUE,
                       verbose = TRUE,
                       sample_types = c("NA", NA, "Life Stage", "Size")[1:2]) {
  if (survey %in% c("NWFSC.Shelf.Rockfish", "NWFSC.Hook.Line")) {
    stop(
      "The catch pull currently does not work for NWFSC Hook & Line Survey data.",
      "\nA subset of the data is available on the data warehouse https://www.webapp.nwfsc.noaa.gov/data",
      "\nContact John Harms (john.harms@noaa.gov) for the full data set."
    )
  }

  if (length(c(common_name, sci_name)) != max(c(length(common_name), length(sci_name)))) {
    stop("Can not pull data using both the common_name or sci_name together.
         \n Please retry using only one.")
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

  perf_codes <- c(
    "operation_dim$legacy_performance_code"
  )

  vars_long <- c(
    "common_name", "scientific_name", "project", "year", "vessel", "tow",
    "total_catch_numbers", "total_catch_wt_kg",
    "subsample_count", "subsample_wt_kg", "cpue_kg_per_ha_der",
    "statistical_partition_dim$statistical_partition_type",
    "partition",
    perf_codes
  )

  # These are the retained and returned fields
  vars_short <- vars_long[!vars_long %in% perf_codes]

  # symbols here are generally: %22 = ", %2C = ",", %20 = " "
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
    message("Pulling catch data. This can take up to ~ 30 seconds (or more).")
  }

  # Pull data from positive tows for selected species
  positive_tows <- try(get_json(url = url_text))
  if (!is.data.frame(positive_tows)) {
    stop()
  }

  # Remove water hauls
  water_hauls <- is.na(positive_tows[, "operation_dim$legacy_performance_code"])
  if (sum(water_hauls) > 0) {
    positive_tows[water_hauls, "operation_dim$legacy_performance_code"] <- -999
  }

  positive_tows <- positive_tows[
    positive_tows[, "statistical_partition_dim$statistical_partition_type"] %in% sample_types,
  ]

  good_tows <- positive_tows[, "operation_dim$legacy_performance_code"] != 8
  positive_tows <- positive_tows[good_tows, ]
  positive_tows <- positive_tows[, vars_short]

  if (sum(is.na(positive_tows[, "common_name"])) > 0) {
    replace <- which(is.na(positive_tows[, "common_name"]))
    positive_tows[replace, "common_name"] <- positive_tows[replace, "scientific_name"]
  }

  # Pull all tow data including tows where the species was not observed
  vars_long <- c(
    "project", "year", "vessel", "pass", "tow", "datetime_utc_iso",
    "depth_m", "longitude_dd", "latitude_dd", "area_swept_ha_der",
    "trawl_id", "operation_dim$legacy_performance_code"
  )

  vars_short <- vars_long[vars_long != "operation_dim$legacy_performance_code"]

  url_text <- get_url(
    data_table = "trawl.operation_haul_fact",
    project_long = project_long,
    years = years,
    vars_long = vars_long
  )

  all_tows <- try(get_json(url = url_text))

  # Remove water hauls
  water_hauls <- is.na(all_tows[, "operation_dim$legacy_performance_code"])
  if (sum(water_hauls) > 0) {
    all_tows[water_hauls, "operation_dim$legacy_performance_code"] <- -999
  }
  keep <- all_tows[, "operation_dim$legacy_performance_code"] != 8
  all_tows <- all_tows[keep, ]
  all_tows <- all_tows[, vars_short]

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

  # Need to check what this is doing
  no_area <- which(is.na(catch$area_swept_ha_der))
  if (length(no_area) > 0) {
    if (verbose) {
      print(
        glue::glue("There are {length(no_area)} records with no area swept calculation. These record will be filled with the mean swept area across all tows.")
      )
      print(
        catch[no_area, c("trawl_id", "year", "area_swept_ha_der", "cpue_kg_per_ha_der", "total_catch_numbers")]
      )
    }
    catch[no_area, "area_swept_ha_der"] <- mean(catch$area_swept_ha_der, trim = 0.05, na.rm = TRUE)
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

  if (sum(c("Life Stage", "Size") %in% sample_types) == 2) {
    n_id <- table(catch$trawl_id)
    if (any(n_id > 0)) {
      warning("Warning: Pulling all sample types (Life Stage and Size) has resulted in multiple records for unique tows (Trawl_id).
      \n The `combine_tows` function can be used to combine these multiple records for unique tows if needed.")
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
