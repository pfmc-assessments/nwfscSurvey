#' Pull biological data (age, length, weight) from the NWFSC data warehouse
#'
#' The website is: https://www.webapps.nwfsc.noaa.gov/data
#' This function can be used to pull a single species or all observed species
#' In order to pull all species leave common_name or sci_name as NULL
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
#' @importFrom dplyr rename
#' @importFrom stringr str_replace_all
#'
#' @examples
#' \dontrun{
#' # SurveyName is only arg that has to be specified
#' bio_dat <- PullBio.fn(SurveyName = "NWFSC.Combo")
#'
#' # Example with specified common name
#' bio_dat <- PullBio.fn(
#'   Name = "vermilion rockfish",
#'   SurveyName = "NWFSC.Combo"
#' )
#'
#' # Example with specified scientific name
#' bio_dat <- PullBio.fn(
#'   SciName = "Eopsetta jordani",
#'   SurveyName = "NWFSC.Combo"
#' )
#'
#' # Example with multiple names
#' bio_dat <- PullBio.fn(
#'   SciName = c("Sebastes aurora", "Eopsetta jordani"),
#'   SurveyName = "NWFSC.Combo"
#' )
# bio_dat <- PullBio.fn(Name = c("Sunset rockfish", "vermilion rockfish",
# "vermilion and sunset rockfish"), SurveyName = "NWFSC.Combo")
#' }
#'
pull_bio <- function(
    survey,
    common_name = NULL,
    sci_name = NULL,
    years = c(1970, 2050),
    dir = NULL,
    convert = TRUE,
    verbose = TRUE,
    standard_filtering = TRUE) {
  options(timeout = 4000000)
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
    var_name <- c("scientific_name", "common_name")
    species <- "pull all"
  }

  project_long <- check_survey(survey = survey)

  if (length(years) == 1) {
    years <- c(years, years)
  }

  vars_long <- c(
    "project", "trawl_id", "common_name", "scientific_name", "year", "vessel", "pass",
    "tow", "datetime_utc_iso", "depth_m", "weight_kg", "ageing_lab", "otosag_id",
    "length_cm", "width_cm", "sex", "age_years", "latitude_dd", "longitude_dd",
    "performance", "station_invalid",
    "ageing_laboratory_dim$laboratory",
    "standard_survey_age_indicator",
    "standard_survey_length_or_width_indicator",
    "standard_survey_weight_indicator",
    "operation_dim$legacy_performance_code"
  )

  species_str <- convert_to_hex_string(species)
  add_species <- paste0("field_identified_taxonomy_dim$", var_name, "|=[", species_str, "]")

  if (any(species == "pull all")) {
    add_species <- ""
  }

  url_text <- get_url(
    data_table = "trawl.individual_fact",
    project_long = project_long,
    add_species = add_species,
    years = years,
    vars_long = vars_long
  )

  if (verbose) {
    cli::cli_alert_info(
      "Pulling biological data for {species}. This can take up to ~ 30 seconds (or more)."
    )
  }
  bio_pull <- try(get_json(url = url_text))

  if (!is.data.frame(bio_pull) & !survey %in% c("AFSC.Slope", "Triennial")) {
    cli::cli_abort(
      "No data returned by the warehouse for the filters given.
      Make sure the year range is correct (cannot include -Inf or Inf) for the
      project selected and the input name is correct, otherwise there may be no
      data for this species from this project.
      URL: {url_text}"
    )
  }

  # This check is needed to proceed on for species where there were no age from
  # the AFSC.Slope and Triennial survey since lengths are checked later in the
  # length_fact data table.
  if (!is.null(dim(bio_pull))) {
    if (survey %in% c("Triennial", "AFSC.Slope")) {
      data_text <- "age/otolith samples"
    } else {
      data_text <-"biological samples"
    }
    bio_pull <- filter_pull(
      data = bio_pull,
      data_type = data_text,
      standard_filtering = standard_filtering,
      verbose = verbose)

    # Filter out non-standard samples
    # Some early entries are NA for standard sample indicators. These should be retained.
    standard_lengths <- bio_pull[, "standard_survey_length_or_width_indicator"] %in% c(NA, "NA", "Standard Survey Length or Width")
    if (length(standard_lengths) != dim(bio_pull)[1]) {
      if (verbose) {
        n <- dim(bio_pull)[1] - length(standard_lengths)
        cli::cli_alert_info(
          "There were {n} lengths that were collected outside standard sampling protocol."
        )
      }
      if (standard_filtering) {
        bio_pull <- bio_pull[standard_lengths, ]
      }
    }

    # Remove non-standard ages
    nonstandard_age <- which(bio_pull[, "standard_survey_age_indicator"] == "Not Standard Survey Age")
    if (length(nonstandard_age) > 0) {
      if (verbose) {
        cli::cli_alert_info(
          "There were {length(nonstandard_age)} ages that were collected outside standard sampling protocol."
        )
      }
      if (standard_filtering) {
        bio_pull[nonstandard_age, "age_years"] <- NA
      }
    }

    # Remove non-standard weights
    nonstandard_wgt <- which(bio_pull[, "standard_survey_weight_indicator"] == "Not Standard Survey Weight")
    if (length(nonstandard_wgt) > 0) {
      if (verbose) {
        cli::cli_alert_info(
          "There were {length(nonstandard_wgt)} weights that were collected outside standard sampling protocol."
        )
      }
      if (standard_filtering) {
        bio_pull[nonstandard_wgt, "weight_kg"] <- NA
      }
    }

    colnames(bio_pull)[colnames(bio_pull) == "standard_survey_length_or_width_indicator"] <- "ageing_lab"
    colnames(bio_pull)[colnames(bio_pull) == "operation_dim$legacy_performance_code"] <- "legacy_performance_code"
    bio_pull$weight <- bio_pull$weight_kg
    bio_pull$age <- bio_pull$age_years
    bio_pull$date <- chron::chron(format(as.POSIXlt(bio_pull$datetime_utc_iso, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
    bio_pull$trawl_id <- as.character(bio_pull$trawl_id)
  }

  bio <- bio_pull

  if (survey %in% c("Triennial", "AFSC.Slope")) {
    url_text <- get_url(
      data_table = "trawl.triennial_length_fact",
      project_long = project_long,
      add_species = add_species,
      years = years,
      vars_long = vars_long
    )
    len_pull <- try(get_json(url = url_text))

    if (is.null(dim(len_pull))) {
      cli::cli_abort(
        "len_pull: No data returned by the warehouse for the filters given.
        Make sure the year range is correct (cannot include -Inf or Inf) for the
        project selected and the input name is correct,otherwise there may be no
        data for this species from this project.
        URL: {url_text}"
      )
    }

    if (is.data.frame(len_pull)) {
      len_pull <- filter_pull(
        data = len_pull,
        data_type = "length samples",
        standard_filtering = standard_filtering,
        verbose = verbose)

      len_pull$weight_kg <- len_pull$weight <- NA
      len_pull$date <- chron::chron(format(as.POSIXlt(len_pull$datetime_utc_iso, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), format = "y-m-d", out.format = "YYYY-m-d")
      len_pull$trawl_id <- as.character(len_pull$trawl_id)
      #len_pull <- len_pull[, colnames(len_pull) != "operation_dim$legacy_performance_code"]
      colnames(len_pull)[colnames(len_pull) == "standard_survey_length_or_width_indicator"] <- "ageing_lab"
      colnames(len_pull)[colnames(len_pull) == "operation_dim$legacy_performance_code"] <- "legacy_performance_code"
    }

    bio <- list()
    if (is.data.frame(len_pull)) {
      bio$length_data <- len_pull
    } else {
      bio$length_data <- "no_lengths_available"
    }
    if (is.data.frame(bio_pull)) {
      bio$age_data <- bio_pull
    } else {
      bio$age_data <- "no_ages_available"
    }
    if (verbose) {
      cli::cli_alert_info(
        "Triennial & AFSC Slope data returned as a list: bio$length_data and bio$age_data"
      )
    }
  }

  if (convert) {
    firstup <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    if (survey %in% c("Triennial", "AFSC.Slope")) {
      if (!is.null(nrow(bio[["length_data"]]))) {
        colnames(bio[["length_data"]]) <- firstup(colnames(bio[["length_data"]]))
      }

      if (!is.null(nrow(bio[["age_data"]]))) {
        colnames(bio[["age_data"]]) <- firstup(colnames(bio[["age_data"]]))
      }
    } else {
      colnames(bio) <- firstup(colnames(bio))
    }
  }

  save_rdata(
    x = bio,
    dir = dir,
    name_base = paste0("bio_", species, "_", survey),
    verbose = verbose
  )

  return(bio)
}
