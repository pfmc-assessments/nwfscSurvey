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

#' Format Species and Project Values for Filenames
#'
#' Build a filename suffix from the first values of the `common_name` and
#' `project` columns. Values are converted to lowercase and spaces are replaced
#' with underscores. Missing columns and empty values are omitted.
#'
#' @param data A data frame that may contain `common_name` and `project`
#'   columns.
#' @return A character string beginning with an underscore when at least one
#'   non-empty value is present, or an empty string otherwise.
#' @keywords internal
#'
format_species_project_suffix <- function(data) {
  # Collect normalized first values from each available filename field.
  filename_values <- vapply(c("common_name", "project"), function(column) {
    if (!column %in% names(data) || nrow(data) == 0) {
      return("")
    }

    value <- as.character(data[[column]][1])
    if (is.na(value) || !nzchar(value)) {
      return("")
    }

    gsub(" ", "_", tolower(value))
  }, character(1))

  filename_values <- filename_values[nzchar(filename_values)]
  if (!length(filename_values)) {
    return("")
  }

  paste0("_", paste(filename_values, collapse = "_"))
}
