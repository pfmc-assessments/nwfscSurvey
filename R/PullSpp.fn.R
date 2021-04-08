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
#' spp <- PullSpp.fn()
#' }
#'
PullSpp.fn <- function() {
  # Get the data from saved .rda file
  PullSpp <- NULL
  newenv <- new.env(hash = TRUE, parent = parent.frame())
  utils::data(PullSpp, package = "nwfscSurvey", envir = newenv)
  PullSpp <- get("PullSpp", envir = newenv)
  return(PullSpp)
}
