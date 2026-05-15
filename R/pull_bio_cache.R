#' Pull biological data (age, length, weight) from pinned data list on posit connect
#'
#' This function can be used to return pinned data for biological samples from
#' NWFSC trawl surveys.
#'
#' @inheritParams pull_catch
#'
#' @author Chantel Wetzel
#' @family data pulling functions
#' @export
#'
#' @import cli
#'
pull_bio_cache <- function(
  common_name = NULL,
  sci_name = NULL,
  survey = "NWFSC.Combo",
  years = c(1970, 2050),
  dir = NULL,
  convert = TRUE,
  verbose = TRUE,
  standard_filtering = TRUE
) {
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
  # data_list <- pins::pin_read(
  #  pins::board_connect(),
  #  "chantel.wetzel/nwfsc_trawl_survey_biological_data"
  # )
  board <- pins::board_url(
    c(
      dataset = "https://connect.fisheries.noaa.gov/nwfsc_trawl_survey_biological_data/"
    )
  )
  data_list <- pins::pin_read(
    board,
    "dataset"
  )
  if (survey == "Triennial") {
    data <- data_list$triennial
  }
  if (survey == "AFSC.Slope") {
    data <- data_list$afsc_slope
  }
  if (survey == "NWFSC.Slope") {
    data <- data_list$nwfsc_slope
  }
  if (survey == "NWFSC.Combo") {
    data <- data_list$wcgbt
  }
  if (survey %in% c("Triennial", "AFSC.Slope")) {
    data_tolower <- list()
    data_tolower$length_data <- data$length_data |>
      dplyr::rename_with(tolower)
    data_tolower$age_data <- data$age_data |>
      dplyr::rename_with(tolower)
  } else {
    data_tolower <- data |>
      dplyr::rename_with(tolower)
  }
  if (!is.null(common_name)) {
    if (survey %in% c("Triennial", "AFSC.Slope")) {
      data_species <- list()
      data_species$length_data <- data_tolower$length_data |>
        dplyr::filter(common_name %in% species)
      data_species$age_data <- data_tolower$age_data |>
        dplyr::filter(common_name %in% species)
    } else {
      data_species <- data_tolower |>
        dplyr::filter(common_name %in% species)
    }
  }
  if (!is.null(sci_name)) {
    if (survey %in% c("Triennial", "AFSC.Slope")) {
      data_species <- list()
      data_species$length_data <- data_tolower$length_data |>
        dplyr::filter(scientific_name %in% species)
      data_species$age_data <- data_tolower$age_data |>
        dplyr::filter(scientific_name %in% species)
    } else {
      data_species <- data_tolower |>
        dplyr::filter(scientific_name %in% species)
    }
  }
  year_range <- years[1]:years[2]
  if (survey %in% c("Triennial", "AFSC.Slope")) {
    data_year <- list()
    data_year$length_data <- data_species$length_data |>
      dplyr::filter(year %in% year_range)
    data_year$age_data <- data_species$age_data |>
      dplyr::filter(year %in% year_range)
  } else {
    data_year <- data_species |>
      dplyr::filter(year %in% year_range)
  }
  if (survey %in% c("Triennial", "AFSC.Slope")) {
    bio <- list()
    bio$length_data <- filter_pull(
      data = data_year$length_data,
      data_type = "length samples",
      standard_filtering = standard_filtering,
      verbose = verbose
    )
    bio$age_data <- filter_pull(
      data = data_year$age_data,
      data_type = "age samples",
      standard_filtering = standard_filtering,
      verbose = verbose
    )
  } else {
    data_text <- "biological samples"
    bio <- filter_pull(
      data = data_year,
      data_type = data_text,
      standard_filtering = standard_filtering,
      verbose = verbose
    )
  }
  if (convert) {
    firstup <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    if (survey %in% c("Triennial", "AFSC.Slope")) {
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
  save_rdata(
    x = bio,
    dir = dir,
    name_base = paste0("bio_", species, "_", survey),
    verbose = verbose
  )
  return(bio)
}
