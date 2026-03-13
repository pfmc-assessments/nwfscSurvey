#' Check and create survey string
#'
#'
#' @inheritParams pull_catch
#'
#'
#' @author Chantel Wetzel
#' @export
#' @family helper function
#'
check_survey <- function(survey) {
  # Survey options available in the data warehouse
  survey_options <- get_survey_names_long()

  # Check the input survey name against available options
  if (sum(!survey %in% survey_options[, 1]) > 0) {
    options <- paste(survey_options[, 1], collapse = "\n")
    cli::cli_abort(
      "The survey argument does not match one of the available options: {options}"
    )
  }

  if (length(survey) == 1) {
    if (!survey %in% survey_options[, 1]) {
      options <- survey_options[, 1]
      cli::cli_abort(
        "The survey name does not match one of the available options: {options}"
      )
    }
  } else {
    if (length(which(survey %in% survey_options[, 1])) != length(survey)) {
      options <- survey_options[, 1]
      cli::cli_abort(
        "One or more of the survey fields does not match one of the available options: {options}"
      )
    }
  }

  # Find the long project name to extract data from the warehouse
  project_long <- NULL
  for (i in 1:length(survey)) {
    tmp <- survey_options[which(survey_options[, 1] %in% survey[i]), 2]
    project_long <- c(project_long, tmp)
  }

  return(project_long)
}
