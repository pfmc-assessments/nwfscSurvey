#' Format NWFSC Hook-and-Line data
#'
#'
#' @param common_name Species names as given in the hook and line data set to
#'   format data for.
#' @param data Data frame of NWFSC Hook-and-Line data.
#' @param verbose A logical that specifies if you want to print messages and
#'   warnings to the console. The default is \code{TRUE}.
#'
#' @return Returns a data frame of formatted hkl data
#' @author Chantel Wetzel
#' @export
#'
#'
format_hkl_bio_data <- function(
  data,
  common_name = NULL,
  verbose = TRUE
) {
  data <- data |>
    dplyr::mutate(
      common_name_lower = tolower(common_name)
    )
  if (!is.null(common_name)) {
    filtered_data <- data |>
      dplyr::filter(common_name_lower == tolower(target_species)) |>
      dplyr::mutate(
        sex = codify_sex(sex),
        age = age_years
      )
  } else {
    filtered_data <- data |>
      dplyr::filter(common_name != "") |>
      dplyr::mutate(
        sex = codify_sex(sex),
        age = age_years
      )
  }

  filtered_data <- filtered_data |>
    dplyr::select(-common_name_lower)

  return(filtered_data)
}
