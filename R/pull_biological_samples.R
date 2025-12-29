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
  standard_filtering = FALSE
) {
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
    "common_name",
    "scientific_name",
    "age_years",
    # "best_available_taxonomy_observation_detail_dim$method_description",
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
    "actual_station_design_dim$reason_station_invalid",
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
    "lab_maturity_detail_dim$biologically_mature_indicator",
    "operation_dim$legacy_performance_code"
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
      "Pulling maturity, stomach, fin clip, and tissue sample data for {species}."
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

  if (verbose) {
    cli::cli_inform(
      "There are {nrow(bio_samples)} biological samples in the pulled data."
    )
  }

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

  bio_samples <- filter_pull(
    data = bio_samples,
    data_type = "biological samples",
    standard_filtering = standard_filtering,
    verbose = verbose
  )

  bio_samples$trawl_id <- as.character(bio_samples$trawl_id)
  rename_columns <- which(colnames(bio_samples) %in% "operation_dim$legacy_performance_code")
  colnames(bio_samples)[rename_columns] <- "legacy_performance_code"
  colnames(bio_samples)[colnames(bio_samples) == "actual_station_design_dim$reason_station_invalid"] <- "reason_station_invalid"

  if (standard_filtering == TRUE & verbose == TRUE) {
    cli::cli_inform(
      "There are {nrow(bio_samples)} biological samples remain after standard filtering."
    )
  }

  save_rdata(
    x = bio_samples,
    dir = dir,
    name_base = paste0("biological_samples_", survey),
    verbose = verbose
  )

  return(bio_samples)
}
