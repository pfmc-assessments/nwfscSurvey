#' Get Survey Abbreviation
#'
#' @details
#' Get abbreviations for a vector of survey names. The input vector of
#' names, which are typically names used to pull the data from the
#' warehouse, do not always match the name agreed upon within the
#' Population Ecology Program. So, standard abbreviations are returned for
#' a given warehouse name.
#' Partial matches are allowed, and are searched
#' for using the first two columns of \code{\link{get_survey_names_long}()}, so be
#' weary of using terms used for multiple surveys as only the first match
#' will be returned for each input value.
#'
#' @param surveys A vector of strings specifying the survey names that you
#' want to get abbreviations for.
#' @param na.return The desired entry you want to replace all \code{is.na()}
#' values with. The default is to return values of \code{""} instead of
#' \code{NA}, though you can change it to actually return NA values by using
#' \code{na.return = NA} as is, i.e., without quoting NA.
#'
#' @export
#' @author Kelli Faye Johnson
#' @return A vector of strings containing a single abbreviation for each
#' input value in \code{surveys}. Only the first match for each survey is
#' returned. The function returns \code{""} for surveys not found in the
#' matrix by default, but this return value for unmatched surveys can
#' be changed by altering \code{na.return}.
#' @seealso See \code{\link{get_survey_names_long}()} for a list of available surveys.
#' @examples
#' # Return a vector of agreed upon abbreviations for the
#' # Triennial Survey and the West Coast Groundfish Bottom Trawl Survey.
#' get_survey_names_abb(c("Triennial", "Combo"))
#' \dontshow{
#' test <- get_survey_names_abb(c("Triennial", "Combo"))
#' testthat::expect_equal(c("Triennial Survey", "NWFSC WCGBTS"), test)
#' test <- get_survey_names_abb(c("Triennial", "bad"))
#' testthat::expect_equal(c("Triennial Survey", ""), test)
#' test <- get_survey_names_abb(c("Triennial", "bad"), na.return = NA)
#' testthat::expect_equal(c("Triennial Survey", NA), test)
#' }
#'
get_survey_names_abb <- function(surveys = "Combo", na.return = "") {
  data <- get_survey_names_long()
  index <- mapply(
    c,
    lapply(surveys, grep, x = data[, 1], ignore.case = TRUE),
    lapply(surveys, grep, x = data[, 2], ignore.case = TRUE),
    SIMPLIFY = FALSE
  )
  out <- data[unlist(lapply(index, "[", 1)), 3]
  out[is.na(out)] <- na.return
  return(out)
}
