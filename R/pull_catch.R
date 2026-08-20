#' Pull catch data for satisfactory tows from the NWFSC data warehouse
#'
#' Pull catch data from the NWFSC data warehouse
#' for a single species or all observed species, where the latter is specified
#' by leaving both `common_name = NULL` and `sci_name = NULL`.
#'
#' @details
#' The data available in the warehouse are cleaned prior to being downloaded
#' with the intent that they provide the best available information for use
#' in an index-standardization procedure.
#'
#' @param common_name A character entry with the desired common name of the
#' species you want to pull data for from the data warehouse.
#' Use a vector of names if you want information for more than one species or
#' if the desired species is included in the database using more than one name,
#' e.g., vermilion rockfish (see the example below).
#' Use the `sci_name` argument if you know the latin name.
#' @param sci_name A character entry with the desired scientific name of the
#' species you want to pull data for from the data warehouse.
#' Use a vector of names if you want information for more than one species or
#' if the desired species is included in the database using more than one name,
#' e.g., vermilion rockfish (see the example below).
#' Use the `common_name` argument if you know the common name.
#' @param years An integer vector of length two with the
#' range of years to pull data for (e.g., c(2003, 2024)).
#' Vector can not contain -Inf or Inf.
#' @param survey A character entry from one of the following options that
#' specifies which survey to pull the data for. The input options are:
#'   * Triennial,
#'   * AFSC.Slope,
#'   * NWFSC.Combo,
#'   * NWFSC.Slope,
#'   * NWFSC.Shelf,
#'   * NWFSC.Hypoxia,
#'   * NWFSC.Santa.Barb.Basin,
#'   * NWFSC.Shelf.Rockfish (not yet working),
#'   * NWFSC.Hook.Line (not yet working),
#'   * NWFSC.Video,
#'   * Triennial.Canada
#' The National Marine Fishery Service Alaska Fisheries Science Center (AFSC)
#' Triennial survey was conducted between 1977 - 2004 occurring every 3rd year.
#' The initial year, 1977, survey is not traditionally used in calculating
#' indices of abundance. The Triennial survey sampled areas within the Canadian
#' EEZ on the West Coast of Vancouver Island in 1980 - 2001 but these data are
#' associated with a different survey name "Triennial.Canada".
#' The AFSC Slope Survey (AFSC.Slope) along the west coast of the U.S. began in 1984 and occurred
#' annually from 1988-2001, with the exception of 1994 and 1998, when surveys were not conducted.
#' Prior to 1997, only a limited portion of the coast was covered in each year.
#' U.S. West Coast groundfish stock assessments only use the four years of consistent
#' and complete survey coverage (1997, 1999-2001). The Northwest Fisheries Science
#' Center (NWFSC) Slope survey (NWFSC.Slope) was conducted between 1998 - 2001.
#' The NWFSC West Coast Groundfish Bottom Trawl survey (NWFSC.Combo) is conducted
#' annually starting in 2003 (excluding 2020) and samples both the U.S. west coast
#' shelf and slope between 55 - 1,280 meters.
#' Data can only be pulled from one survey at a time, though we are working on
#' allowing for a vector of survey names.
#' Currently, `NWFSC.Shelf.Rockfish` and `NWFSC.Hook.Line` are not supported.
#' @param dir Directory where output will be saved. The directory where the file should be saved.
#' If dir = NULL no output will be saved.
#' @param convert TRUE/FALSE to convert column names to first letter uppercase
#' which aligns with the expected names in data processing functions.
#' @param verbose A logical that specifies if you want to print messages and
#'   warnings to the console. The default is `TRUE`.
#' @param sample_types A character vector of sample types, i.e.,
#' `"statistical_partition_dim"`, that you would like to keep. The default is
#' to only keep `NA` values, both real and character NA. But, for some
#' instances you may want to keep Life Stage and Size samples. The majority
#' of samples with `"statistical_partition_dim"` of Size and Life Stage are
#' Pacific hake and should not be considered different than regular survey
#' samples. The other types of samples that may be designated Life Stage are
#' egg cases that can be caught and identified for select elasmobranch
#' species. These type of samples should not be included in the data used
#' to estimate indices of abundance and are omitted by default.
#' @param standard_filtering A logical TRUE/FALSE that specifies whether data
#'   should be filtered using the standard filtering which removes tows with bad
#'   performance (water haul or poor net performance), or stations that have been
#'   removed from the survey sampling protocol.
#'
#' @author Chantel Wetzel
#' @export
#' @family data pulling functions
#'
#' @import chron
#' @import cli
#' @importFrom stringr str_replace_all
#' @importFrom dplyr left_join rename
#'
#' @examples
#' \dontrun{
#' # Pull catch for all species
#' catch_data <- pull_catch()
#'
#' # Example with specified common name
#' catch_data <- pull_catch(
#'   common_name = "vermilion rockfish"
#' )
#'
#' # Example with specified scientific name
#' catch_data <- pull_catch(
#'   sci_name = "Eopsetta jordani"
#' )
#'
#' # Example with multiple names
#' catch_data <- pull_catch(common_name = c(
#'   "vermilion rockfish",
#'   "vermilion and sunset rockfish"
#' ))
#'
#' catch_data <- pull_catch(
#'   sci_name = c(
#'     "Sebastes miniatus",
#'     "Sebastes sp. (crocotulus)",
#'     "Sebastes sp. (miniatus / crocotulus)"
#'   ),
#'   survey = "Triennial"
#' )
#' }
#'
pull_catch <- function(
  common_name = NULL,
  sci_name = NULL,
  survey = "NWFSC.Combo",
  years = c(1980, 2050),
  dir = NULL,
  convert = TRUE,
  verbose = TRUE,
  sample_types = c(NA, "Not Recorded / Unspecified", "Life Stage", "Size")[1:2],
  standard_filtering = TRUE
) {
  if (survey %in% c("NWFSC.Shelf.Rockfish", "NWFSC.Hook.Line")) {
    cli::cli_abort(
      "The catch pull currently does not work for NWFSC Hook & Line Survey data.",
      "These data can be accessed by using pull_hkl_cache()."
    )
  }

  if (
    length(c(common_name, sci_name)) !=
      max(c(length(common_name), length(sci_name)))
  ) {
    cli::cli_abort(
      "Function is unable to pull data using both the common_name or sci_name together.
      Please retry using only one."
    )
  }

  check_dir(dir = dir, verbose = verbose)

  if (is.null(common_name)) {
    var_name <- "best_available_taxon_scientific_name"
    species <- sci_name
  } else {
    var_name <- "best_available_taxon_common_name"
    species <- common_name
  }
  if (is.null(sci_name) & is.null(common_name)) {
    var_name <- "best_available_taxon_common_name"
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
    "best_available_taxon_common_name",
    "best_available_taxon_scientific_name",
    "nmfs_project_name",
    "survey_year",
    "vessel_name",
    "pass_number",
    "total_catch_individuals_count",
    "total_catch_weight_kg",
    "bottom_trawl_operation_key",
    "sampled_catch_individuals_count",
    "sampled_catch_weight_kg",
    "catch_per_unit_effort_kg_per_ha",
    "tow_performance_name",
    "actual_station_current_deactivation_reasons",
    "is_actual_station_currently_active",
    "life_stage_name"
  )

  species_str <- convert_to_hex_string(species)
  add_species <- paste0(
    var_name,
    "=",
    species_str
  )

  if (any(species == "pull all")) {
    add_species <- ""
  }

  url_text <- get_url(
    data_table = "catch",
    project_long = project_long,
    add_species = add_species,
    years = years,
    vars_long = vars_long
  )

  if (verbose) {
    cli::cli_alert_info(
      "Pulling catch data for {species}."
    )
  }

  # Pull data from positive tows for selected species
  positive_tows <- try(get_json(url = url_text), silent = TRUE)
  if (inherits(positive_tows, "try-error")) {
    cli::cli_alert_danger(
      "The data request failed. The data warehouse may be offline. Please use pull_catch_cache() to access data."
    )
    cli::cli_abort("")
  }
  if (!is.data.frame(positive_tows)) {
    cli::cli_abort(
      "There are no tows where {species} was caught."
    )
  } else {
    if (verbose) {
      cli::cli_alert_info(
        "There are {nrow(positive_tows)} positive tows across all years pulled."
      )
    }
  }
  positive_tows_convert <- convert_colnames(
    x = positive_tows
  )
  positive_tows_filtered <- filter_pull(
    data = positive_tows_convert,
    data_type = "positive tows",
    standard_filtering = standard_filtering,
    verbose = verbose
  )

  bad_sample_types <- which(
    !positive_tows_filtered[,
      "partition_sample_types"
    ] %in%
      sample_types
  )
  if (length(bad_sample_types) > 0) {
    if (verbose) {
      cli::cli_alert_info(
        "There were {length(bad_sample_types)} positive tows where the sample type was not requested (e.g., Life Stage, Size)."
      )
    }
    positive_tows_filtered <- positive_tows_filtered[-bad_sample_types, ]
  }

  if (sum(is.na(positive_tows_filtered[, "common_name"])) > 0) {
    replace <- which(is.na(positive_tows_filtered[, "common_name"]))
    positive_tows_filtered[replace, "common_name"] <- positive_tows_filtered[
      replace,
      "scientific_name"
    ]
  }

  # Pull all tow data including tows where the species was not observed
  vars_long <- c(
    "nmfs_project_name",
    "survey_year",
    "vessel_name",
    "pass_number",
    "tow_sequence_number",
    "bottom_trawl_operation_key",
    "sampling_date",
    "on_bottom_seafloor_depth_m",
    "best_tow_longitude_dd",
    "best_tow_latitude_dd",
    "seafloor_area_swept_ha",
    "tow_performance_name",
    "actual_station_current_deactivation_reasons",
    "is_actual_station_currently_active"
  )

  url_text <- get_url(
    data_table = "tows",
    project_long = project_long,
    years = years,
    vars_long = vars_long
  )
  all_tows <- try(get_json(url = url_text))
  all_tows_convert <- convert_colnames(
    x = all_tows
  )
  all_tows_filtered <- filter_pull(
    data = all_tows_convert,
    data_type = "tows",
    standard_filtering = standard_filtering,
    verbose = FALSE
  )

  all_tows_filtered <- all_tows_filtered[
    !duplicated(all_tows_filtered$trawl_id),
  ]

  positive_tows_grouped <- dplyr::group_by(
    .data = positive_tows_filtered,
    common_name,
    scientific_name
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
  names_intersect <- intersect(
    colnames(all_tows_filtered),
    colnames(positive_tows_filtered)
  )
  zero_tows <- purrr::map_df(
    .x = positive_tows_split,
    .f = \(y) {
      dplyr::anti_join(x = all_tows_filtered, y = y, by = names_intersect)
    },
    .id = "groups"
  ) |>
    tidyr::separate_wider_delim(
      cols = "groups",
      delim = "_",
      names = colnames(group_names)
    )

  # Join the positive tows with the tow information
  positive_tows_with_tow_info <- dplyr::left_join(
    x = positive_tows_filtered,
    y = all_tows_filtered,
    by = intersect(
      colnames(all_tows_filtered),
      colnames(positive_tows_filtered)
    )
  )
  # Join the augmented positive tow information with the zero tows
  # arrange by common_name and tow_id
  catch <- dplyr::full_join(
    x = positive_tows_with_tow_info,
    y = zero_tows,
    by = c(colnames(group_names), colnames(all_tows_filtered))
  ) |>
    dplyr::arrange(common_name, trawl_id)
  colnames(catch)[
    colnames(catch) == "actual_station_design_dim$reason_station_invalid"
  ] <- "reason_station_invalid"
  colnames(catch)[
    colnames(catch) == "statistical_partition_dim$statistical_partition_type"
  ] <- "partition_sample_types"
  colnames(catch)[
    colnames(catch) == "operation_dim$legacy_performance_code"
  ] <- "legacy_performance_code"

  good_depth <- which(catch[, "depth_m"] >= 55 & catch[, "depth_m"] <= 1280)
  if (length(good_depth) != dim(catch)[1]) {
    if (verbose) {
      n <- length(catch[-good_depth, "total_catch_numbers"] > 0)
      cli::cli_alert_info(
        "There were {n} positive tows that are outside the standard depth range."
      )
    }
    if (standard_filtering) {
      catch <- catch[good_depth, ]
    }
  }

  no_area <- which(is.na(catch$area_swept_ha_der))
  if (length(no_area) > 0) {
    if (verbose) {
      n <- length(no_area)
      cli::cli_alert_info(
        "There were {n} tows with no area swept calculation and will be filled with the mean swept area across all tows."
      )
    }
    #if (standard_filtering) {
    #  catch[no_area, "area_swept_ha_der"] <- mean(
    #    catch$area_swept_ha_der,
    #    trim = 0.05,
    #    na.rm = TRUE
    #  )
    #}
  }

  # Fill in zeros where needed
  catch <- catch |>
    dplyr::mutate(
      cpue_kg_per_ha_der = dplyr::if_else(
        condition = is.na(cpue_kg_per_ha_der),
        true = 0,
        false = cpue_kg_per_ha_der
      ),
      cpue_kg_km2 = cpue_kg_per_ha_der * 100,
      total_catch_numbers = dplyr::if_else(
        condition = is.na(total_catch_numbers) & is.na(total_catch_wt_kg),
        true = 0,
        false = total_catch_numbers
      ),
      total_catch_wt_kg = dplyr::if_else(
        condition = total_catch_numbers == 0 & is.na(total_catch_wt_kg),
        true = 0,
        false = total_catch_wt_kg
      ),
      subsample_count = dplyr::if_else(
        condition = is.na(subsample_count) & is.na(subsample_wt_kg),
        true = 0,
        false = subsample_count
      ),
      subsample_wt_kg = dplyr::if_else(
        condition = subsample_count == 0 & is.na(subsample_wt_kg),
        true = 0,
        false = subsample_wt_kg
      ),
      trawl_id = as.character(trawl_id)
    ) |>
    dplyr::rename(area_swept_ha = area_swept_ha_der)

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

  if (standard_filtering == TRUE & verbose == TRUE) {
    n <- sum(catch[, "total_catch_wt_kg"] > 0, na.rm = TRUE)
    cli::cli_alert_info(
      "There are {n} positive tows remaining across all years after standard filtering."
    )
  }

  save_rdata(
    x = catch,
    dir = dir,
    name_base = paste0("catch_", species, "_", survey),
    verbose = verbose
  )

  return(catch)
}
