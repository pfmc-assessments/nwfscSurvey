#' Format NWFSC Hook-and-Line catch data
#'
#' This function will format hook-and-line catch to account for all observations
#' of the `common_name`, if provided. If `return_positive_sites_only` is TRUE,
#' the the data will be filtered down remove sampling sites that have never
#' observed the select species.  If no `common_name` is provided, then an
#' unfiltered data set is returned.
#'
#' @param data Data frame of NWFSC Hook-and-Line data.
#' @param common_name Species names as given in the hook and line data set to
#'   format data for.
#' @param return_positive_site_only Logical that specifies whether data from sites
#'   that have observed common_name at least once across the time series are
#'   returned.The default is \code{TRUE}.
#' @param verbose A logical that specifies if you want to print messages and
#'   warnings to the console. The default is \code{TRUE}.
#'
#' @return Returns a data frame of formatted hook-and-line catch data frame
#' @author Chantel Wetzel
#' @export
#'
#'
format_hkl_catch_data <- function(
  data,
  common_name = NULL,
  return_positive_sites_only = TRUE,
  verbose = TRUE
) {
  data <- data |>
    dplyr::mutate(
      common_name_lower = tolower(common_name)
    )
  if (!is.null(common_name)) {
    target_species <- common_name
    if (tolower(common_name) %in% data[["common_name_lower"]]) {
      data <- data |>
        dplyr::mutate(
          n_caught_of_target_species = dplyr::case_when(
            common_name_lower == tolower(target_species) ~ 1,
            .default = 0
          ),
          target_species = target_species
        )
    } else {
      cli::cli_abort(
        "The common_name was not found in the data frame: {common_name}"
      )
    }
  } else {
    data <- data |>
      dplyr::mutate(
        n_caught_of_target_species = 0,
        target_species = common_name
      )
  }

  total_sample_by_site <- data |>
    dplyr::summarise(
      .by = c("site_number"),
      n_years_site_sampled = length(unique(year))
    ) |>
    dplyr::arrange(site_number) |>
    as.data.frame()

  combine_data <- dplyr::left_join(
    data,
    total_sample_by_site,
    by = "site_number"
  ) |>
    dplyr::group_by(site_number) |>
    dplyr::mutate(
      n_caught_of_target_species_by_site_all_years = sum(
        n_caught_of_target_species
      )
    )
  if (!is.null(common_name) & return_positive_sites_only) {
    removed_sites <- combine_data |>
      dplyr::filter(n_caught_of_target_species_by_site_all_years == 0) |>
      dplyr::summarise(
        sites = unique(site_number)
      )
    if (verbose) {
      site_message <- ifelse(
        dim(removed_sites)[1] > 0,
        removed_sites$site_number,
        "no sites removed"
      )
      cli::cli_alert_info(
        "Filtering data for only sites with > 0 of {common_name} across all years. Removing data from the following site(s): {site_message}.
        To turn off site filtering set return_positive_sites_only = FALSE."
      )
    }
    filtered_data <- combine_data |>
      dplyr::filter(n_caught_of_target_species_by_site_all_years > 0)
  } else {
    filtered_data <- combine_data
  }

  formatted_data <- filtered_data |>
    dplyr::mutate(
      crew = as.factor(as.character(
        paste0(angler_first_name, "_", angler_last_name)
      )),
      area_name = as.factor(area_name),
      hook_number = as.factor(hook_number),
      angler_number = as.factor(angler_number),
      year = as.factor(year),
      site_number = as.factor(site_number),
      vessel = as.factor(vessel),
      drop_number = as.factor(drop_number)
    ) |>
    dplyr::select(
      -common_name_lower,
      -n_years_site_sampled,
      -n_caught_of_target_species_by_site_all_years
    ) |>
    as.data.frame()
  return(formatted_data)
}
