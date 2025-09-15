#' Utility function used throughout the package
#'
#' @details
#' Function that converts a string to a hex string
#' for common name or scientific name when pulling
#' data. This function is used within the pull_*
#' functions that retrieve species specific data
#'
#' @param x A string of either common_name or
#' sci_name
#' @author Kelli Johnson
#'
#' @examples
#' \dontrun{
#' common_name <- c("lingcod", "sablefish", "Pacific cod")
#' convert_to_hex_string(common_name)
#' }
#'
convert_to_hex_string <- function(x) {
  hex_comma <- toupper(paste0("%", charToRaw(",")))
  hex_quote <- paste0("%", charToRaw('"'))
  hex_space <- paste0("%", charToRaw(" "))
  stopifnot(inherits(x, "character"))

  # Convert spaces to %20
  x_no_spaces <- gsub(pattern = " ", replacement = hex_space, x)

  # Wrap each string in quotes with %22 and
  # separate strings with %2C, which is a comma
  out <- paste0(hex_quote, x_no_spaces, hex_quote, collapse = hex_comma)

  return(out)
}
