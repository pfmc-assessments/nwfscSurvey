#' Function to create url to pull data from the data warehouse
#'
#'
#' @inheritParams pull_catch
#' @param data_table table to pull from the data warehouse, options =
#' trawl.catch_fact, trawl.operation_haul_fact
#' @param project_long survey project name
#' @param add_species string of species name created by the [pull_catch()] or [pull_bio()]
#' functions.
#' @param vars_long string of fields to pull from the data warehouse
#'
#' @author Chantel Wetzel
#' @export
#'
#' @import glue
#'
#'
get_url <- function(data_table, project_long, add_species, years, vars_long) {
  if (years[1] != years[2]) {
    year_str <- glue::glue(
      "survey_year=bw:{years[1]}~{years[2]}"
    )
  } else {
    year_str <- glue::glue("survey_year=eq:{years[1]}")
  }

  if (missing(add_species)) {
    add_species <- ""
  } else {
    add_species <- paste0(add_species, collapse = "~")
  }

  if (missing(project_long)) {
    project_str <- ""
  } else {
    x_no_spaces <- gsub(pattern = " ", replacement = "+", project_long)
    project_str <- paste0(
      "nmfs_project_name=in:",
      paste0(x_no_spaces, collapse = "~")
    )
  }

  url_text <- paste0(
    "https://www.webapps.nwfsc.noaa.gov/data-catalog/api/v1/bottom-trawl/",
    data_table,
    "?$data_format=json",
    "&fields=",
    glue::glue_collapse(vars_long, sep = ","),
    "&",
    project_str,
    "&",
    add_species,
    "&",
    year_str
  )

  return(url_text)
}
