#' 
#'
#' @details
#' 
#'
#' @param url The url to pull the data
#'
#' @author Kelli F. Johnson
#' @export
#' @md
#'
#' @import jsonlite
#' @import glue
#' @import httr
#'
get_json <- function(url) {
  out <- httr::GET(url) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON()
  if (!(is.data.frame(out) && NROW(out) > 0)) {
    stop(glue::glue("No data returned by the warehouse using {url}"))
  }
  return(out)
}