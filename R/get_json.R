#' Get json content from a URL
#'
#' @details
#' Get information stored on the web in .json format using a URL. The content
#' is first pulled from the web as text with UTF-8 encoding. Then the text
#' is passed to [jsonlite::fromJSON()]. This workflow ensures that the URL
#' is not mistaken for a file name rather than web content.
#'
#' @param url A string containing a valid URL to pull the data from the data
#'  warehouse.
#'
#' @author Kelli F. Johnson
#' @export
#' @return A data frame.
#' @seealso See all the `pull_*` functions for examples where this function is
#' used, e.g., [pull_catch()].
#'
get_json <- function(url) {
  out <- httr::GET(url) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  return(out)
}
