#' Check and create survey string
#'
#' 
#' @template survey
#'
#'
#' @author Chantel Wetzel
#' @export
#'
check_survey <- function(survey){

  # Survey options available in the data warehouse
  survey_options <- createMatrix()

  # Check the input survey name against available options
  if (sum(!survey %in% survey_options[, 1]) > 0) {
    stop(
      "The survey argument does not match one of the available options:\n",
      paste(survey_options[, 1], collapse = "\n")
    )
  }

  if (length(survey) == 1) {
    if (!survey %in% survey_options[, 1]) {
      stop(cat("The survey name does not match one of the available options:", survey_options[, 1]))
    }
  } else {
    if (length(which(survey %in% survey_options[, 1])) != length(survey)) {
      stop(cat("One or more of the survey fields does not match one of the available options:", survey_options[, 1]))
    }
  }

  # Find the long project name to extract data from the warehouse
  project_long <- NULL
  for (i in 1:length(survey)) {
    tmp <- survey_options[which(survey_options[, 1] %in% survey[i]), 2]
    project_long  <- c(project_long, tmp)
  }

  return(project_long)

}