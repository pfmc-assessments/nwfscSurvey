#' Pull species names from the warehouse
#'
#' Pull common name and scientific name information from the
#' data warehouse.
#' The website is https://www.webapps.nwfsc.noaa.gov/data
#'
#' @author Kelli Faye Johnson
#' @export
#'
#' @examples
#' \dontrun{
#' spp <- pull_spp()
#' }
#'
pull_spp <- function() {
  # Get the data from saved .rda file
  species <- NULL
  newenv <- new.env(hash = TRUE, parent = parent.frame())
  utils::data(PullSpp, package = "nwfscSurvey", envir = newenv)
  species <- get("PullSpp", envir = newenv)
  return(species)
}
