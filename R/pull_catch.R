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
#' # survey is only arg that has to be specified
#' catch_data <- pull_catch(survey = "NWFSC.Combo")
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
  years = c(1970, 2050),
  dir = NULL,
  convert = TRUE,
  verbose = TRUE,
  sample_types = c("NA", NA, "Life Stage", "Size")[1:2],
  standard_filtering = TRUE
) {
  if (survey %in% c("NWFSC.Shelf.Rockfish", "NWFSC.Hook.Line")) {
    cli::cli_abort(
      "The catch pull currently does not work for NWFSC Hook & Line Survey data.",
      "Contact John Harms (john.harms@noaa.gov) for the full data set."
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
    "common_name",
    "scientific_name",
    "project",
    "year",
    "vessel",
    "tow",
    "total_catch_numbers",
    "total_catch_wt_kg",
    "trawl_id",
    "subsample_count",
    "subsample_wt_kg",
    "cpue_kg_per_ha_der",
    "statistical_partition_dim$statistical_partition_type",
    "partition",
    "operation_dim$legacy_performance_code",
    "performance",
    "station_invalid",
    "actual_station_design_dim$reason_station_invalid",
    "depth_m"
  )

  # These are the retained and returned fields
  # vars_short <- vars_long[!vars_long %in% perf_codes]

  species_str <- convert_to_hex_string(species)
  add_species <- paste0(
    "field_identified_taxonomy_dim$",
    var_name,
    "|=[",
    species_str,
    "]"
  )

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
      "Pulling catch data for {species}."
    )
  }

  # Pull data from positive tows for selected species
  positive_tows <- try(get_json(url = url_text))
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

  positive_tows <- filter_pull(
    data = positive_tows,
    data_type = "positive tows",
    standard_filtering = standard_filtering,
    verbose = verbose
  )

  bad_sample_types <- which(
    !positive_tows[,
      "statistical_partition_dim$statistical_partition_type"
    ] %in%
      sample_types
  )
  if (length(bad_sample_types) > 0) {
    if (verbose) {
      cli::cli_alert_info(
        "There were {length(bad_sample_types)} positive tows where the sample type was not requested (e.g., Life Stage, Size)."
      )
    }
    positive_tows <- positive_tows[-bad_sample_types, ]
  }
  if (sum(is.na(positive_tows[, "common_name"])) > 0) {
    replace <- which(is.na(positive_tows[, "common_name"]))
    positive_tows[replace, "common_name"] <- positive_tows[
      replace,
      "scientific_name"
    ]
  }

  positive_tows <- positive_tows[, colnames(positive_tows) != "depth_m"]

  # Pull all tow data including tows where the species was not observed
  vars_long <- c(
    "project",
    "year",
    "vessel",
    "pass",
    "tow",
    "datetime_utc_iso",
    "depth_hi_prec_m",
    "longitude_dd",
    "latitude_dd",
    "area_swept_ha_der",
    "trawl_id",
    "operation_dim$legacy_performance_code",
    "performance",
    "station_invalid",
    "actual_station_design_dim$reason_station_invalid"
  )

  url_text <- get_url(
    data_table = "trawl.operation_haul_fact",
    project_long = project_long,
    years = years,
    vars_long = vars_long
  )

  all_tows <- try(get_json(url = url_text))

  colnames(all_tows)[(colnames(all_tows) == "depth_hi_prec_m")] <- "depth_m"

  if (standard_filtering == TRUE & verbose == TRUE) {
    cli::cli_alert_info(
      "There are {nrow(positive_tows)} positive tows remaining across all years after standard filtering."
    )
  }

  all_tows <- filter_pull(
    data = all_tows,
    data_type = "tows",
    standard_filtering = standard_filtering,
    verbose = FALSE
  )

  all_tows <- all_tows[
    !duplicated(paste(
      all_tows$year,
      all_tows$pass,
      all_tows$vessel,
      all_tows$tow
    )),
  ]

  positive_tows_grouped <- dplyr::group_by(
    .data = positive_tows,
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
  colnames(catch)[
    colnames(catch) == "actual_station_design_dim$reason_station_invalid"
  ] <- "reason_station_invalid"
  colnames(catch)[
    colnames(catch) == "statistical_partition_dim$statistical_partition_type"
  ] <- "partition_sample_types"
  colnames(catch)[
    colnames(catch) == "operation_dim$legacy_performance_code"
  ] <- "legacy_performance_code"

  no_area <- which(is.na(catch$area_swept_ha_der))
  if (length(no_area) > 0) {
    if (verbose) {
      n <- length(no_area)
      cli::cli_alert_info(
        "There were {n} tows with no area swept calculation and will be filled with the mean swept area across all tows."
      )
    }
    if (standard_filtering) {
      catch[no_area, "area_swept_ha_der"] <- mean(
        catch$area_swept_ha_der,
        trim = 0.05,
        na.rm = TRUE
      )
    }
  }

  # Fill in zeros where needed
  catch <- catch |>
    dplyr::mutate(
      cpue_kg_per_ha_der = dplyr::case_when(
        is.na(cpue_kg_per_ha_der) ~ 0,
        .default = cpue_kg_per_ha_der
      ),
      cpue_kg_km2 = dplyr::case_when(
        is.na(cpue_kg_per_ha_der) ~ 0,
        .default = cpue_kg_per_ha_der * 100
      ),
      total_catch_numbers = dplyr::case_when(
        is.na(total_catch_numbers) & is.na(total_catch_wt_kg) ~ 0,
        .default = total_catch_numbers
      ),
      total_catch_wt_kg = dplyr::case_when(
        total_catch_numbers == 0 & is.na(total_catch_wt_kg) ~ 0,
        .default = total_catch_wt_kg
      ),
      subsample_count = dplyr::case_when(
        is.na(subsample_count) & is.na(subsample_wt_kg) ~ 0,
        .default = subsample_count
      ),
      subsample_wt_kg = dplyr::case_when(
        subsample_count == 0 & is.na(subsample_wt_kg) ~ 0,
        .default = subsample_wt_kg
      ),
      trawl_id = as.character(trawl_id),
      date = chron::chron(
        format(
          as.POSIXlt(datetime_utc_iso, format = "%Y-%m-%dT%H:%M:%S"),
          "%Y-%m-%d"
        ),
        format = "y-m-d",
        out.format = "YYYY-m-d"
      )
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
