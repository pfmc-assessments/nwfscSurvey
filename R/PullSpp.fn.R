#' Pull Species Names From the Warehouse
#'
#' Pull common name and scientific name information from the
#' data warehouse.
#' The website is https://www.webapps.nwfsc.noaa.gov/data
#'
#' @author Kelli Faye Johnson
#' @export
#'
#' @examples
#'\dontrun{
#' spp <- PullSpp.fn()
#'}

PullSpp.fn <- function () {
  # Get the data using an already existing function
  project <- createMatrix()
  project <- project[project[, 1] == "NWFSC.Combo", 2]
  UrlText <- paste0("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/",
    "trawl.catch_fact/selection.json?filters=",
    "project=", gsub(" ", "%20", project, " "),
    ",station_invalid=0,performance=Satisfactory,",
    "depth_ftm>=30,depth_ftm<=700,",
    "date_dim$year>=2000",
    "&variables=common_name,scientific_name")
  DataPull <- try(jsonlite::fromJSON(UrlText))

  # Subset for unique species names
  spp <- DataPull[
    !duplicated(DataPull[, "scientific_name"]) &
    !grepl("unident", DataPull[, "common_name"]), ]
  # Deal with "and" species
  andspp <- spp[
    grepl(" and ", spp[, "common_name"]) &
    !grepl("unident", spp[, "common_name"]), ]
  andsplit <- data.frame(
    common_name = paste(unlist(strsplit(
      gsub(" rockfish", "", andspp[, "common_name"]), " and ")), "rockfish"),
    scientific_name = paste("Sebastes", unlist(strsplit(
      gsub("Sebastes sp\\. \\(|\\)", "", andspp[, "scientific_name"]), " / ")))
  )
  spp <- merge(spp, andsplit, all = TRUE)

  # Add new columns
  spp[, "latin"] <- spp[, "scientific_name"]
  spp[, "common"] <- spp[, "common_name"]
  spp[, "scientific_name"] <- gsub(" ", "_", spp[, "latin", drop = TRUE])
  spp[, "common_name"] <- gsub(" ", "_", tolower(spp[, "common", drop = TRUE]))
  return(spp)
}
