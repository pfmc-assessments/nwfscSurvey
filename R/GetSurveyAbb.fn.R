#' Get Survey Abbreviation
#'
#' Get abbreviations for a vector of survey names. Standard abbreviations
#' do not always match the name used to extract the information from the
#' data warehouse, so this function will look up standarized names that
#' were agreed upon within the Population Ecology Program.
#' Partial matches are allowed, though may return multiple
#' values and only the first match is returned.
#' 
#' @param surveys A vector of strings specifying the survey names that you
#' want to get abbreviations for.
#'
#' @export
#' @author Kelli Faye Johnson
#' @return A vector of strings containing a single abbreviation for each
#' input value in \code{surveys}. Only the first match for each survey is
#' returned. Values of NA were not found in the look-up matrix.
#' @seealso See \code{\link{createMatrix}} for a list of available surveys.
#' @examples
#' GetSurveyAbb.fn(c("Triennial", "Combo"))

GetSurveyAbb.fn <- function(surveys = "Combo") {
  data <- createMatrix()
  index <- mapply(c,
    lapply(surveys, grep, x = data[, 1], ignore.case = TRUE),
    lapply(surveys, grep, x = data[, 2], ignore.case = TRUE)
  )
  data[unlist(lapply(index, "[", 1)), 3]
}
