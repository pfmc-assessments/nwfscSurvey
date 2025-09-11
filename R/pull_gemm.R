#' Pull Groundfish Expanded Multiyear Mortality data
#'
#' The data are being pulled from: https://connect.fisheries.noaa.gov/gemm_csv/
#' This function can be used to pull all gemm data, a single species, or a
#' subset of species c("Canary Rockfish", "Widow Rockfish"). Species names in
#' the gemm are capitalized (e.g. Canary Rockfish). However, there are checks in
#' the function to adjust input species names if the input names do not match
#' the expected capitalization (e.g. "canary rockfish", "canary_rockfish"). The
#' function also allows you to subset the data by year using the years input and
#' to save the object if the dir function input is given.
#'
#' @template common_name
#' @template years
#' @template dir
#' @template verbose
#'
#' @author Chantel Wetzel
#' @export
#'
#' @import dplyr
#' @import stringr
#' @import janitor
#'
#' @examples
#' \dontrun{
#' # Pull all GEMM data
#' all_data <- pull_gemm()
#'
#' # Pull for a specific specis
#' widow_data <- pull_gemm(common_name = "widow rockfish")
#'
#' # Pull multiple species
#' data <- pull_gemm(common_name = c("Widow Rockfish", "Canary Rockfish"))
#'
#' # Pull species and subset years
#' widow_recent <- pull_gemm(common_name = "Widow Rockfish", years = 2014:2019)
#' }
#'
pull_gemm <- function(
    common_name,
    years,
    dir = NULL,
    verbose = TRUE) {
  check_dir(dir = dir, verbose = verbose)

  # Pull all gemm data
  gemm <- pins::pin_read(pins::board_connect(), "kayleigh.somers/gemmdatcsv")  |>
    janitor::clean_names()

  # Clean up the common_name if necessary
  if (!missing(common_name)) {
    format_common_name <- sub("_", " ", common_name)
    format_common_name <- stringr::str_to_title(format_common_name)
    gemm <- gemm |>
      dplyr::filter(species %in% format_common_name)
  }

  # Check the years if provided
  if (!missing(years)) {
    if (sum(years %in% gemm$year) == 0) {
      cli::cli_abort(
        "The input years were not found in the available gemm years: {years}."
      )
    }
    if(length(years) == 2 & (max(years) - min(years)) > 1) {
      cli::cli_inform(
        "Only two years of data being returned: {years}.
        The expected form of years is a vector (e.g., 2012:2018) which will return all years within the vector."
      )
    }
    gemm <- gemm |>
      dplyr::filter(year %in% years)
  }

  if (!is.null(dir)) {
    save(gemm, file = file.path(dir, "gemm_data.rdata"))
  }
  return(gemm)
}
