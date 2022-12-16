#' Pull catch data for satisfactory tows from the NWFSC data warehouse
#'
#' Pull catch data from the
#' [NWFSC data warehouse](https://www.webapps.nwfsc.noaa.gov/data)
#' for a single species or all observed species, where the latter is specified
#' by leaving both `common_name = NULL` and `sci_name = NULL`.
#'
#' @details
#' The data available in the warehouse are cleaned pior to being downloaded
#' with the intent that they provide the best available information for use
#' in an index-standardization procedure. The removed samples may be of use
#' to others with a less-restrictive goal than producing an index of abundance.
#' For example, life-stage samples are excluded because they are not collected
#' using the same protocols as standard samples.
#' To download all data, we currently recommend going to the
#' [NWFSC data warehouse](https://www.webapps.nwfsc.noaa.gov/data)
#' and using the csv link to extract data for a single species at a time.
#' In the future, we hope to add functionality to this package such that
#' downloading all data can be done easily within this function.
#' See [Issue #43](https://github.com/pfmc-assessments/nwfscSurvey/issues/43)
#' for more information.
#'
#' @template common_name
#' @template sci_name
#' @template years
#' @template survey
#' @template dir 
#' @template convert
#' @template verbose 
#'
#' @author Chantel Wetzel
#' @export
#'
#' @import chron
#' @importFrom stringr str_replace_all
#' @importFrom dplyr left_join rename
#' @import glue
#'
#' @examples
#' \dontrun{
#' # survey is only arg that has to be specified
#' catch_data <- pull_catch(survey = "NWFSC.Combo")
#'
#' # Example with specified common name
#' catch_data <- pull_catch(common_name = "vermilion rockfish",
#'                  survey = "NWFSC.Combo")
#'
#' # Example with specified scientific name
#' catch_data <- pull_catch(sci_name = "Eopsetta jordani",
#'                  survey = "NWFSC.Combo")
#'
#' # Example with multiple names
#' catch_data <- pull_catch(common_name = c("vermilion rockfish",
#'                  "vermilion and sunset rockfish"), survey = "NWFSC.Combo")
#'
#' catch_data <- pull_catch(sci_name = c("Sebastes miniatus",
#'                  "Sebastes sp. (crocotulus)", 
#'                  "Sebastes sp. (miniatus / crocotulus)"),
#'                  survey = "NWFSC.Combo")
#' }
#'
pull_catch <- function(common_name = NULL, 
                       sci_name = NULL, 
                       years = c(1980, 2050), 
                       survey = NULL, 
                       dir = NULL, 
                       convert = TRUE,
                       verbose = TRUE) {

  if (survey %in% c("NWFSC.Shelf.Rockfish", "NWFSC.Hook.Line")) {
    stop("The catch pull currently does not work for NWFSC Hook & Line Survey data.",
      "\nA subset of the data is available on the data warehouse https://www.webapp.nwfsc.noaa.gov/data",
      "\nContact John Harms (john.harms@noaa.gov) for the full data set.") 
  }

  check_dir(dir = dir, verbose = verbose)

  if (is.null(common_name)) {
    var_name <- "scientific_name"
    species <- sci_name
  }
  if (is.null(sci_name)) {
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

  perf_codes <- c(
    "operation_dim$legacy_performance_code", 
    "statistical_partition_dim$statistical_partition_type"
  )

  vars_long <- c(
    "common_name", "scientific_name", "project", "year", "vessel", "tow",      
    "total_catch_numbers", "total_catch_wt_kg",  
    "subsample_count", "subsample_wt_kg",  "cpue_kg_per_ha_der",
    perf_codes
  )

  # These are the retained and returned fields
  vars_short <- vars_long[!vars_long %in% perf_codes]

  # symbols here are generally: %22 = ", %2C = ",", %20 = " "
  species_str <- paste0(
    "%22",stringr::str_replace_all(species[1]," ","%20"),"%22"
  )

  if(length(species) > 1) {
    for(i in 2:length(species)) {
      species_str <- paste0(
        species_str, "%2C", paste0(
        "%22",stringr::str_replace_all(species[i]," ","%20"),"%22"))
    }
  }
  add_species <- paste0("field_identified_taxonomy_dim$", var_name, "|=[", species_str,"]")
  
  if (species[1] == "pull all") {
    add_species <- ""
  }

  url_text <- get_url(data_table = "trawl.catch_fact",
                      project_long = project_long,
                      add_species = add_species,
                      years = years,
                      vars_long = vars_long)

  if (verbose) {
    message("Pulling catch data. This can take up to ~ 30 seconds (or more).")
  }

  # Pull data from positive tows for selected species
  positive_tows <- try(get_json(url = url_text))
  if (!is.data.frame(positive_tows)) {
    stop(cat("\nNo data returned by the warehouse for the filters given.
              \n Make sure the year range is correct for the project selected and the input name is correct,
              \n otherwise there may be no data for this species from this project.\n"))
  }

  # Remove water hauls
  water_hauls <- is.na(positive_tows[, "operation_dim$legacy_performance_code"])
  if (sum(water_hauls) > 0) {
    positive_tows[water_hauls, "operation_dim$legacy_performance_code"] <- -999
  }

  # Retain on standard survey samples
  # whether values are NA or "NA" varies based on the presence of "Life Stage" samples
  standard_samples <- sum(is.na(positive_tows[, "statistical_partition_dim$statistical_partition_type"])) != nrow(positive_tows)
  if (standard_samples) {
    keep <- positive_tows[, "statistical_partition_dim$statistical_partition_type"] == "NA"
    positive_tows <- positive_tows[keep, ]
  }

  good_tows <- positive_tows[, "operation_dim$legacy_performance_code"] != 8
  positive_tows <- positive_tows[good_tows, ]
  positive_tows <- positive_tows[, vars_short]

  # Pull all tow data including tows where the species was not observed
  vars_long <- c("project", "year", "vessel", "pass", "tow", "datetime_utc_iso", 
                 "depth_m", "longitude_dd", "latitude_dd", "area_swept_ha_der", 
                 "trawl_id", "operation_dim$legacy_performance_code")

  vars_short <- vars_long[vars_long != "operation_dim$legacy_performance_code"]

  url_text <- get_url(data_table = "trawl.operation_haul_fact",
                      project_long = project_long,
                      years = years,
                      vars_long = vars_long)

  all_tows <- try(get_json(url = url_text)) 

  # Remove water hauls
  water_hauls <- is.na(all_tows[, "operation_dim$legacy_performance_code"])
  if (sum(water_hauls) > 0) {
    all_tows[water_hauls, "operation_dim$legacy_performance_code"] <- -999
  }
  keep <- all_tows[, "operation_dim$legacy_performance_code"] != 8
  all_tows <- all_tows[keep, ]
  all_tows <- all_tows[, vars_short]

  all_tows <- all_tows[
    !duplicated(paste(all_tows$year, all_tows$pass, all_tows$vessel, all_tows$tow)),
    c("project", "trawl_id", "year", "pass", "vessel", "tow", "datetime_utc_iso", "depth_m", 
      "longitude_dd", "latitude_dd", "area_swept_ha_der"
    )
  ]

  # Link each data set together based on trawl_id
  if (species == "pull all"){
    grid <- expand.grid(
      "trawl_id" = unique(all_tows$trawl_id), 
      "common_name" = unique(positive_tows$common_name),
      stringsAsFactors = FALSE
    )     
  } else {
    grid <- expand.grid(
      "trawl_id" = unique(all_tows$trawl_id), 
      "common_name" = unique(positive_tows$common_name),
      "scientific_name" = unique(positive_tows$scientific_name),
      stringsAsFactors = FALSE
    )    
  }

  catch_data <- dplyr::left_join(grid, all_tows)
  catch <- dplyr::left_join(catch_data, positive_tows)

  # Need to check what this is doing
  no_area <- which(is.na(catch$area_swept_ha_der))
  if (length(no_area) > 0) {
    if (verbose) {
      print(
        glue::glue("There are {length(no_area)} records with no area swept calculation. These record will be filled with the mean swept area across all tows."))
      print(
        catch[no_area, c("trawl_id", "year", "area_swept_ha_der", "cpue_kg_per_ha_der", "total_catch_numbers")])
    }
    catch[no_area, "area_swept_ha_der"] <- mean(catch$area_swept_ha_der, trim = 0.05, na.rm = TRUE)
  }

  # Fill in zeros where needed 
  catch[is.na(catch)] <- 0

  catch$date_formatted <- chron::chron(
    format(as.POSIXlt(catch$datetime_utc_iso, format = "%Y-%m-%dT%H:%M:%S"), "%Y-%m-%d"), 
    format = "y-m-d", out.format = "YYYY-m-d")

  catch$trawl_id <- as.character(catch$trawl_id)
  catch$cpue_kg_km2 <- catch$cpue_kg_per_ha_der * 0.01

  if(convert) {
    catch$Area_Swept_ha <- catch$area_swept_ha_der
    catch$date <- catch$date_formatted
    firstup <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    colnames(catch) <- firstup(colnames(catch))
    colnames(catch)[colnames(catch)=="Cpue_kg_km2"] <- "cpue_kg_km2"
    colnames(catch)[colnames(catch)=="Cpue_kg_per_ha_der"] <- "cpue_kg_per_ha_der"
    colnames(catch)[colnames(catch)=="Total_catch_numbers"] <- "total_catch_numbers"
    colnames(catch)[colnames(catch)=="Total_catch_wt_kg"] <- "total_catch_wt_kg"
  }

  save_rdata(
    x = catch,
    dir = dir,
    name_base = paste0("catch_", species, "_", survey),
    verbose = verbose
  )

  return(catch)
}
