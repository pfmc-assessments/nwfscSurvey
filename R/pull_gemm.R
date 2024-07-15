#' Pull gemm data from the NWFSC data warehouse
#'
#' The website is: https://www.webapps.nwfsc.noaa.gov/data
#' This function can be used to pull all gemm data, a single species, or a
#' subset of species c("Canary Rockfish", "Widow Rockfish"). Species names in
#' the gemm are capitalized (e.g. Canary Rockfish). However, there are checks in
#' the function to adjust input species names if the input names do not match
#' the expected capitalization (e.g. "canary rockfish", "canary_rockfish"). The
#' fuction also allows you to subset the data by year using the years input and to
#' save the object if the dir function input is given.
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
  gemm <- utils::read.csv(
    url("https://www.webapps.nwfsc.noaa.gov/data/api/v1/source/observer.gemm_fact/selection.csv"),
    encoding = "UTF-8-BOM"
  ) %>%
    janitor::clean_names()

  # Check the species name if provided
  if (!missing(common_name)) {
    tmp <- NULL
    for (ii in 1:length(common_name)) {
      new_name <- sub("_", " ", common_name[ii])
      new_name <- stringr::str_to_title(new_name)
      find <- which(gemm$species == new_name)
      if (length(find) == 0) {
        stop(cat("The species name was not found: ", new_name))
      }
      tmp <- rbind(tmp, gemm[find, ])
    }
    gemm <- tmp
  }

  # Check the years if provided
  if (!missing(years)) {
    if (sum(years %in% gemm$year) == 0) {
      stop(cat("The input years were not found in the available gemm years: ", min(gemm$year), "-", max(gemm$year)))
    }
    gemm <- gemm[gemm$year %in% years, ]
  }

  if (!is.null(dir)) {
    if (missing(common_name)) {
      save(gemm, file = paste0(dir, "/gemm_out.Rdat"))
    } else {
      save_name <- sub(" ", "_", common_name)
      save(gemm, file = paste0(dir, "/gemm_", save_name, ".rdata"))
    }
  }

  return(gemm)
}
