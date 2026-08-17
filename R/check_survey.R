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
  if (!any(survey %in% survey_options)) {
    cli::cli_abort(
      "The survey argument does not match one of the available options: {survey_options}"
    )
  }

  # Find the long project name to extract data from the warehouse
  project_long <- NULL
  for (i in 1:length(survey)) {
    find <- c(
      grep(survey[i], survey_options[, "old_names"]),
      grep(survey[i], survey_options[, "new_names"]),
      grep(survey[i], survey_options[, "alt_names"])
    )
    tmp <- survey_options[find, "new_names"]
    project_long <- c(project_long, tmp)
  }
  return(unname(project_long))
}
