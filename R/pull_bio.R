#' Pull biological data (age, length, weight) from the NWFSC data warehouse
#'
#' This function can be used to pull a single species or all observed species
#' In order to pull all species leave common_name or sci_name as NULL
#'
#' @inheritParams pull_catch
#'
#' @author Chantel Wetzel
#' @family data pulling functions
#' @export
#'
#' @import chron
#' @import cli
#' @importFrom dplyr rename
#' @importFrom stringr str_replace_all
#'
#' @examples
#' \dontrun{
#' bio_data <- pull_bio(common_name = "sablefish")
#' }
#'
pull_bio <- function(
  common_name = NULL,
  sci_name = NULL,
  survey = "NWFSC.Combo",
  years = c(1980, 2050),
  dir = NULL,
  convert = TRUE,
  verbose = TRUE,
  standard_filtering = TRUE
) {
  options(timeout = 4000000)
  if (survey %in% c("NWFSC.Shelf.Rockfish", "NWFSC.Hook.Line")) {
    cli::cli_abort(
      "The biological data pull currently does not work for NWFSC Hook & Line Survey data.",
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
    var_name <- c(
      "best_available_taxon_scientific_name",
      "best_available_taxon_common_name"
    )
    species <- "pull all"
  }

  project_long <- check_survey(survey = survey)

  if (length(years) == 1) {
    years <- c(years, years)
  }

  vars_long <- c(
    "best_available_taxon_common_name",
    "best_available_taxon_scientific_name",
    "nmfs_project_name",
    "survey_year",
    "vessel_name",
    "pass_number",
    "bottom_trawl_operation_key",
    "sampling_date",
    "on_bottom_seafloor_depth_m",
    "best_tow_latitude_dd",
    "best_tow_longitude_dd",
    "tow_performance_name",
    "actual_station_current_deactivation_reasons",
    "is_actual_station_currently_active",
    "specimen_size_cm",
    "specimen_size_sample_type_name",
    #"width_cm",
    "specimen_weight_kg",
    "specimen_sex_code",
    "specimen_age_years",
    "ageing_lab_name",
    "specimen_age_sample_label",
    #"standard_survey_age_indicator",
    #"standard_survey_length_or_width_indicator",
    #"standard_survey_weight_indicator",
    "specimen_ovary_sample_label",
    "ovary_last_analyzed_at",
    "ovary_proportion_atresia",
    "lab_analyzed_maturity_stage_name",
    "specimen_finclip_sample_label",
    "specimen_tissue_sample_label",
    "specimen_stomach_sample_label",
    "specimen_life_stage_name"
  )

  #NEED TO FIGURE OUT HOW TO IDENTIFY WATER HAULS

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
    data_table = "specimens",
    project_long = project_long,
    add_species = add_species,
    years = years,
    vars_long = vars_long
  )

  if (verbose) {
    cli::cli_alert_info(
      "Pulling biological data for {species}."
    )
  }
  bio_pull <- try(get_json(url = url_text))
  if (inherits(bio_pull, "try-error")) {
    cli::cli_alert_danger(
      "The data request failed. The data warehouse may be offline. Please use pull_bio_cache() to access data."
    )
    cli::cli_abort("")
  }

  if (
    !is.data.frame(bio_pull) &
      !survey %in%
        c(
          "Triennial",
          "AFSC.Slope",
          "AFSC/RACE Triennial Groundfish Shelf Survey",
          "AFSC/RACE Triennial Groundfish Shelf Survey (by NWFSC)",
          "AFSC/RACE Slope Survey"
        )
  ) {
    cli::cli_abort(
      "No data returned by the warehouse for the filters given.
      Make sure the year range is correct (cannot include -Inf or Inf) for the
      project selected and the input name is correct, otherwise there may be no
      data for this species from this project.
      URL: {url_text}"
    )
  }

  if (is.data.frame(bio_pull)) {
    bio_pulls_convert <- convert_colnames(
      x = bio_pull
    )
  } else {
    bio_pulls_convert <- NULL
  }

  # This check is needed to proceed on for species where there were no age from
  # the AFSC.Slope and Triennial survey since lengths are checked later in the
  # length_fact data table.
  if (is.null(bio_pulls_convert)) {
    if (
      survey %in%
        c(
          "Triennial",
          "AFSC.Slope",
          "AFSC/RACE Triennial Groundfish Shelf Survey",
          "AFSC/RACE Triennial Groundfish Shelf Survey (by NWFSC)",
          "AFSC/RACE Slope Survey"
        )
    ) {
      data_text <- "age/otolith samples"
      bio_pull_filtered <- NULL
    }
    if (verbose) {
      cli::cli_alert_info(
        "There were 0 {data_text} pulled."
      )
    }
  } else {
    data_text <- "biological samples"

    if (verbose) {
      cli::cli_alert_info(
        "There were {nrow(bio_pulls_convert)} {data_text} pulled."
      )
    }
    bio_pull_filtered <- filter_pull(
      data = bio_pulls_convert,
      data_type = data_text,
      standard_filtering = standard_filtering,
      verbose = verbose
    )

    # Filter out non-standard samples
    # Some early entries are NA for standard sample indicators. These should be retained.
    # standard_lengths <- bio_pull_filtered[,
    #   "standard_survey_length_or_width_indicator"
    # ] %in%
    #   c(NA, "NA", "Standard Survey Length or Width")
    # if (length(standard_lengths) != dim(bio_pull_filtered)[1]) {
    #   if (verbose) {
    #     n <- dim(bio_pull_filtered)[1] - length(standard_lengths)
    #     cli::cli_alert_info(
    #       "There were {n} lengths that were collected outside standard sampling protocol."
    #     )
    #   }
    #   if (standard_filtering) {
    #     bio_pull_filtered<- bio_pull_filtered[standard_lengths, ]
    #   }
    # }

    # Remove non-standard ages
    # nonstandard_age <- which(
    #   bio_pull_filtered[, "standard_survey_age_indicator"] == "Not Standard Survey Age"
    # )
    # if (length(nonstandard_age) > 0) {
    #   if (verbose) {
    #     cli::cli_alert_info(
    #       "There were {length(nonstandard_age)} ages that were collected outside standard sampling protocol."
    #     )
    #   }
    #   if (standard_filtering) {
    #     bio_pull_filtered[nonstandard_age, "age_years"] <- NA
    #   }
    # }

    # Remove non-standard weights
    # nonstandard_wgt <- which(
    #   bio_pull[, "standard_survey_weight_indicator"] ==
    #     "Not Standard Survey Weight"
    # )
    # if (length(nonstandard_wgt) > 0) {
    #   if (verbose) {
    #     cli::cli_alert_info(
    #       "There were {length(nonstandard_wgt)} weights that were collected outside standard sampling protocol."
    #     )
    #   }
    #   if (standard_filtering) {
    #     bio_pull_filtered[nonstandard_wgt, "weight_kg"] <- NA
    #   }
    # }

    colnames(bio_pull_filtered)[
      colnames(bio_pull_filtered) ==
        "actual_station_design_dim$reason_station_invalid"
    ] <- "reason_station_invalid"
    bio_pull_filtered$trawl_id <- as.character(bio_pull_filtered$trawl_id)
  }
  bio <- bio_pull_filtered

  if (
    survey %in%
      c(
        "Triennial",
        "AFSC.Slope",
        "AFSC/RACE Triennial Groundfish Shelf Survey",
        "AFSC/RACE Triennial Groundfish Shelf Survey (by NWFSC)",
        "AFSC/RACE Slope Survey"
      )
  ) {
    url_text <- get_url(
      data_table = "triennial-specimen-lengths",
      project_long = project_long,
      add_species = add_species,
      years = years,
      vars_long = vars_long
    )
    len_pull <- try(get_json(url = url_text))
    len_pull_convert <- convert_colnames(
      x = len_pull
    )
    len_pull_convert[, "date"] <- chron::chron(
      format(
        as.POSIXlt(
          len_pull_convert[, "date"],
          format = "%Y-%m-%dT%H:%M:%S"
        ),
        "%Y-%m-%d"
      ),
      format = "y-m-d",
      out.format = "YYYY-m-d"
    )
    if (is.null(dim(len_pull_convert))) {
      cli::cli_abort(
        "len_pull: No data returned by the warehouse for the filters given.
        Make sure the year range is correct (cannot include -Inf or Inf) for the
        project selected and the input name is correct,otherwise there may be no
        data for this species from this project.
        URL: {url_text}"
      )
    }

    if (is.data.frame(len_pull_convert)) {
      if (verbose) {
        cli::cli_alert_info(
          "There were {nrow(len_pull)} length samples pulled."
        )
      }
      len_pull_filtered <- filter_pull(
        data = len_pull_convert,
        data_type = "length samples",
        standard_filtering = standard_filtering,
        verbose = verbose
      )

      len_pull_filtered$weight_kg <- NA
      len_pull_filtered$trawl_id <- as.character(len_pull_filtered$trawl_id)
      colnames(len_pull_filtered)[
        colnames(len_pull_filtered) ==
          "actual_station_design_dim$reason_station_invalid"
      ] <- "reason_station_invalid"
    }

    bio <- list()
    if (is.data.frame(len_pull_filtered)) {
      bio$length_data <- len_pull_filtered
    } else {
      bio$length_data <- "no_lengths_available"
    }
    if (is.data.frame(bio_pull_filtered)) {
      bio$age_data <- bio_pull_filtered
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
    if (
      survey %in%
        c(
          "Triennial",
          "AFSC.Slope",
          "AFSC/RACE Triennial Groundfish Shelf Survey",
          "AFSC/RACE Triennial Groundfish Shelf Survey (by NWFSC)",
          "AFSC/RACE Slope Survey"
        )
    ) {
      if (!is.null(nrow(bio[["length_data"]]))) {
        colnames(bio[["length_data"]]) <- firstup(colnames(bio[[
          "length_data"
        ]]))
      }

      if (!is.null(nrow(bio[["age_data"]]))) {
        colnames(bio[["age_data"]]) <- firstup(colnames(bio[["age_data"]]))
      }
    } else {
      colnames(bio) <- firstup(colnames(bio))
    }
  }

  if (
    survey %in%
      c(
        "Triennial",
        "AFSC.Slope",
        "AFSC/RACE Triennial Groundfish Shelf Survey",
        "AFSC/RACE Triennial Groundfish Shelf Survey (by NWFSC)",
        "AFSC/RACE Slope Survey"
      )
  ) {
    if (standard_filtering == TRUE & verbose == TRUE) {
      n_len <- ifelse(
        length(nrow(bio[["length_data"]])) > 0,
        nrow(bio[["length_data"]]),
        0
      )
      n_age <- ifelse(
        length(nrow(bio[["age_data"]])) > 0,
        nrow(bio[["age_data"]]),
        0
      )
      cli::cli_alert_info(
        "There were {n_len} lengths and {n_age} ages samples remaining after applying standard filtering."
      )
    }
  } else {
    if (standard_filtering == TRUE & verbose == TRUE) {
      cli::cli_alert_info(
        "There were {nrow(bio)} biological samples remaining after applying standard filtering."
      )
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
