#' Function to create url to pull data from the data warehouse
#'
#'
#'
#' @param data_table table to pull from the data warehouse, options =
#' trawl.catch_fact, trawl.operation_haul_fact
#' @param project_long survey project name
#' @param add_species string of species name created by the pull_catch or pull_bio
#' functions.
#' @template years
#' @param vars_long string of fields to pull from the data warehouse
#'
#' @author Chantel Wetzel
#' @export
#'
#' @import glue
#'
#'
get_url <- function(data_table, project_long, add_species, years, vars_long) {
  year_str <- glue::glue("date_dim$year>={years[1]},date_dim$year<={years[2]}")

  if (missing(add_species)) {
    add_species <- ""
  }
  if (add_species != "") {
    add_species <- paste0(add_species, ",")
  }


  if (data_table %in% c("trawl.individual_fact", "trawl.triennial_length_fact", "trawl.operation_haul_fact")) {
    year_str <- glue::glue("year>={years[1]},year<={years[2]}")
  }

  if (missing(project_long)) {
    project_str <- ""
  } else {
    project_str <- paste0(
      "project=",
      paste(strsplit(project_long, " ")[[1]], collapse = "%20")
    )
  }

  url_text <- paste0(
    "https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/",
    # "https://www.devwebapps.nwfsc.noaa.gov/data/api/v1/source/",
    data_table,
    "/selection.json?filters=",
    project_str, ",",
    #"station_invalid=0,",
    #"performance=Satisfactory,",
    "depth_ftm>=30,depth_ftm<=700,",
    add_species,
    year_str,
    "&variables=",
    glue::glue_collapse(vars_long, sep = ",")
  )

  return(url_text)
}
