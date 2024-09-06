#' Pull biological sample information from the NWFSC data warehouse for biological
#' collections taken at sea. Generally these are samples that require lab processing.
#' Generally, these types of biological sample are maturity, stomach, fin clips, and
#' tissue samples. This function returns collection information for these samples
#' include the sample numbers which allows the lab analysis to be linked back to
#' the sampled fish.
#' The website is: https://www.webapps.nwfsc.noaa.gov/data.
#'
#' @template common_name
#' @template sci_name
#' @template years
#' @template survey
#' @template dir
#' @template verbose
#' @param standard_filtering A logical TRUE/FALSE that specifies whether data
#'   should be filtered using the standard filtering which removes tows with bad
#'   performance (water haul or poor net performance), or stations that have been
#'   removed from the survey sampling protocol.
#'
#' @return Returns a data frame of special biological samples with sample number
#' @author Chantel Wetzel
#' @export
#'
#' @import cli
#' @import glue
#'
#'
pull_biological_samples <- function(
    survey,
    common_name = NULL,
    sci_name = NULL,
    years = c(1980, 2050),
    dir = NULL,
    verbose = TRUE,
    standard_filtering = FALSE) {
  # increase the timeout period to avoid errors when pulling data
  options(timeout = 4000000)

  if (length(c(common_name, sci_name)) != max(c(length(common_name), length(sci_name)))) {
    cli::cli_abort(
      "Can not pull data using both the common_name or sci_name together.
       Please retry using only one."
    )
  }

  check_dir(dir = dir, verbose = verbose)

  project_long <- check_survey(survey = survey)

  if (length(years) == 1) {
    years <- c(years, years)
  }

  if (is.null(common_name)) {
    var_name <- "scientific_name"
    species <- sci_name
  } else {
    var_name <- "common_name"
    species <- common_name
  }
  if (is.null(sci_name) & is.null(common_name)) {
    var_name <- c("scientific_name", "common_name")
    species <- "pull all"
  }

  species_str <- convert_to_hex_string(species)
  add_species <- paste0("field_identified_taxonomy_dim$", var_name, "|=[", species_str, "]")

  if (any(species == "pull all")) {
    add_species <- ""
  }

  vars_long <- c(
    "common_name", "scientific_name",
    "age_years",
    #"best_available_taxonomy_observation_detail_dim$method_description",
    "best_available_taxonomy_observation_detail_whid",
    "date_yyyymmdd",
    "depth_m",
    "individual_tracking_id",
    "lab_maturity_detail_dim",
    "latitude_dd",
    "left_pectoral_fin_id",
    "leg",
    "length_cm",
    "length_type",
    "longitude_dd",
    "max_depth_m",
    "min_depth_m",
    "most_recent_age_update",
    "most_recent_maturity_update_date_whid",
    "most_recent_taxon_update",
    "otosag_id",
    "ovary_id",
    "ovary_proportion_atresia",
    "partition",
    "pass",
    "performance",
    "program",
    "project",
    "reason_stn_invalid",
    "sex",
    "species_category",
    "species_subcategory",
    "station_invalid",
    "stomach_id",
    "taxon_rank",
    "taxon_source",
    "tissue_id",
    "tow",
    "trawl_id",
    "vessel",
    "vessel_id",
    "weight_kg",
    "width_cm",
    "width_type",
    "year",
    "year_stn_invalid",
    "lab_maturity_detail_dim$biologically_mature_certain_indicator",
    "lab_maturity_detail_dim$biologically_mature_indicator"
  )

  url_text <- get_url(
    data_table = "trawl.individual_fact",
    project_long = project_long,
    add_species = add_species,
    years = years,
    vars_long = vars_long
  )

  if (verbose) {
    cli::cli_alert_info(
      "Pulling maturity, stomach, fin clip, and tissue sample data."
    )
  }
  bio_samples <- try(get_json(url = url_text))

  keep <- which(
    bio_samples$ovary_id > 0 |
    bio_samples$stomach_id > 0 |
    bio_samples$tissue_id > 0 |
    bio_samples$left_pectoral_fin_id > 0
  )
  bio_samples <- bio_samples[keep, ]

  rename_columns <- which(
    colnames(bio_samples) %in%
      c(
        "lab_maturity_detail_dim$biologically_mature_certain_indicator",
        "lab_maturity_detail_dim$biologically_mature_indicator"
      )
  )

  colnames(bio_samples)[rename_columns] <-
    c(
      "biologically_mature_certain_indicator",
      "biologically_mature_indicator"
    )

  bio_samples[bio_samples == "NA"] <- NA

  # Remove tows outside of standard depths 55-1,280 m
  good_depth <- which(bio_samples$depth_m >= 55 & bio_samples$depth_m <= 1280)
  if (length(good_depth) != dim(bio_samples)[1]) {
    if (verbose) {
      n <- dim(bio_samples)[1] - length(good_depth)
      cli::cli_alert_info(
        "There were {n} biological samples collected in tows outside standard depth range (55-1,280 m)."
      )
    }
    if (standard_filtering) {
      bio_samples <- bio_samples[good_depth, ]
    }
  }

  # Now start filtering out tows that have issues:
  good_performance <- which(bio_samples$performance == "Satisfactory")
  if (length(good_performance) != dim(bio_samples)[1]) {
    if (verbose) {
      n <- length(which(bio_samples$performance != "Satisfactory"))
      cli::cli_alert_info(
        "There were {n} biological samples collected in tows with poor performance (e.g., no area swept estimate, net issues, etc.)."
      )
    }
    if (standard_filtering) {
      bio_samples <- bio_samples[good_performance, ]
    }
  }

  good_station <- which(bio_samples$station_invalid == 0)
  if (length(good_station) != dim(bio_samples)[1]) {
    if (verbose) {
      n <- length(which(bio_samples$station_invalid != 0))
      cli::cli_alert_info(
        "There were {n} biological samples collected at stations that have being removed from the standard station list."
      )
    }
    if (standard_filtering) {
      bio_samples <- bio_samples[good_station, ]
    } else {
      bio_samples[good_station, "station_invalid"] <- "good_station"
    }
  }

  save_rdata(
    x = bio_samples,
    dir = dir,
    name_base = paste0("biological_samples_", survey),
    verbose = verbose
  )

  return(bio_samples)
}
