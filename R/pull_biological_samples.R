#' Pull biological sample information from the NWFSC data warehouse. The types
#' of biological sample informaiton returned are maturity, stomach, fin clips, 
#' and tissue.
#' samples. If a fish has 
#' The website is: https://www.webapps.nwfsc.noaa.gov/data.
#'

#' @template common_name
#' @template sci_name
#' @template years 
#' @template survey 
#' @template dir 
#' @template verbose 
#'
#' @return Returns a data frame of special biological samples with sample number
#' @author Chantel Wetzel
#' @export
#'
#' @import glue
#'
#'
pull_biological_samples <- function(common_name = NULL, 
                                   sci_name = NULL,
                                   years= c(1980, 2050), 
                                   survey = NULL, 
                                   dir = NULL, 
                                   verbose = TRUE) {

  # increase the timeout period to avoid errors when pulling data
  options(timeout = 4000000)

  check_dir(dir = dir, verbose = verbose)  

  project_long <- check_survey(survey = survey)

  if (length(years) == 1) {
    years <- c(years, years)
  }

  if (is.null(common_name)) {
    var_name <- "scientific_name"
    species <- sci_name
  }
  if (is.null(sci_name)) {
    var_name <- "common_name"
    species <- common_name
  }
  if (is.null(sci_name) & is.null(common_name)) {
    var_name <- c("scientific_name", "common_name")
    species <- "pull all"
  }

  # symbols here are generally: %22 = ", %2C = ",", %20 = " "
  species_str <- paste0("%22",stringr::str_replace_all(species[1]," ","%20"),"%22")
  if(length(species) > 1) {
    for(i in 2:length(species)) {
      species_str <- paste0(species_str, "%2C", paste0("%22",stringr::str_replace_all(species[i]," ","%20"),"%22"))
    }
  }
  add_species <- paste0("field_identified_taxonomy_dim$", var_name, "|=[", species_str,"]")
  
  if (species[1] == "pull all") {
    add_species <- ""
  }

  vars_str <- c(
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

  url_text <- paste0(
    "https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/",
    "trawl.individual_fact",
    "/selection.json?filters=",
     paste0("project=",paste(strsplit(project_long, " ")[[1]], collapse = "%20")),
     ",", add_species,
    ",year>", years[1], ",year<", years[2], 
    #",ovary_id>0&",
    "&variables=", 
    glue::glue_collapse(vars_str, sep = ",")
  )

  if (verbose) {
    message("Pulling maturity, stomach, fin clip, and tissue sample data.")
  }
  bio_samples <- try(get_json(url = url_text))
  
  keep <- which(bio_samples$ovary_id > 0 | bio_samples$stomach_id > 0 |
      bio_samples$tissue_id > 0 | bio_samples$left_pectoral_fin_id > 0)
  bio_samples <- bio_samples[keep, ]

  if (!is.null(dir)) {
    time <- substring(Sys.time(), 1, 10)
    save(bio_samples, file = file.path(dir, paste("biological_samples_", survey, "_", time, ".rda", sep = "")))
    if (verbose) {
      message(
        glue::glue("Biological sample data file saved to following location: {dir}"))
    }
  }

  return(bio_samples)
}