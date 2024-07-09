#' Calculates design based estimates from survey data for West Coast surveys.
#'
#' @details
#' The design based index is calculated based on the area of the strata with the
#' output estimates representing the adjusted median estimates
#' (e.g., est * exp(0.5*log(var)). This function returns a list of design-based
#' estimates by strata and estimates combined across stratas by year. This
#' function is designed to work with data frames pulled from the NWFSC
#' data warehouse using [pull_catch()].
#' See: Gunderson, D.R. and Sample, T.M. 1980. Distribution and abundance of rockfish off Washington,
#' Oregon, and California during 1977. Marine Fisheries Review: March - April.
#'
#' @param data Data frame of catch data that has been created by the [pull_catch()].
#' @template strata
#' @param CI A numerical value that specifies the confidence interval to return.
#'   Values should be between 0.01 to 0.99.
#' @template dir
#' @template printfolder
#' @template month
#' @template fleet
#' @template verbose
#'
#' @returns List of biomass estimates by year and biomass estimates by year and
#' strata. The biomass estimates are in metric tons.
#'
#' @author Allan Hicks and Chantel Wetzel
#' @importFrom glue glue
#' @importFrom stats optim qnorm sd var
#' @importFrom utils write.csv
#' @export
#'
#' @examples
#' \dontrun{
#' catch <- pull_catch(
#'   common_name = "petrale sole",
#'   survey = "NWFSC.Combo"
#' )
#'
#' strata <- CreateStrataDF.fn(
#'   names = c("shallow_wa", "shallow_or", "shallow_ca", "deep_wa", "deep_or", "deep_ca"),
#'   depths.shallow = c( 55,   55,   55,  183,  183, 183),
#'   depths.deep    = c(183,  183,  183,  549,  549, 549),
#'   lats.south     = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
#'   lats.north     = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0))
#'
#' biommass <- get_design_based(
#'   data = catch,
#'   strata = strata
#' )
#'
#' }
#'
get_design_based <- function(
  data,
  strata,
  CI = 0.95,
  dir = NULL,
  month = NA,
  fleet = NA,
  printfolder = "forSS3",
  verbose = TRUE)
{

  plotdir <- file.path(dir, printfolder)
  check_dir(dir = plotdir, verbose = verbose)

  colnames(data) <- tolower(colnames(data))
  strata_vars <- c("depth_m", "latitude_dd")
  colnames(strata) <- tolower(colnames(strata))

  if (sum(strata_vars %in% colnames(data)) != length(strata_vars)) {
    stop(glue::glue(
      "The {strata_vars[1]} and/or {strata_vars[2]} were not found in the data.
      They can be either uppper or lower case."))
  }

  if (nrow(strata) == 1) {
    warning("It is recommended to have more than one strata, consider revising strata.")
  }

  if (is.null(data[, "cpue_kg_km2"])) {
    stop("There must be a column called cpue_kg_km2 in the data")
  }
  data[, "cpue_mt_km2"] <- data[, "cpue_kg_km2"] / 1000

  # create strata factors
  colnames(strata)[1] <- "stratum"
  stratum <- rep(NA, nrow(data)) # the stratum factor
  for (n in 1:nrow(strata)) {
    ind <- rep(TRUE, nrow(data))
    for (s in 1:length(strata_vars)) {
      ind <- ind &
        data[, strata_vars[s]] >= strata[n, paste(strata_vars[s], ".1", sep = "")] &
        data[, strata_vars[s]] <  strata[n, paste(strata_vars[s], ".2", sep = "")]
    }
    stratum[ind] <- as.character(strata[n, 1])
  }
  data <- data.frame(data, stratum)

  # add area for each stratum to data frame
  data_joined <- dplyr::left_join(data, strata[, c("stratum", "area")])

  biomass_year_stratum <- data_joined |>
    dplyr::filter(!is.na(stratum)) |>
    dplyr::group_by(year, stratum) |>
    dplyr::summarize(
      ntows = dplyr::n(),
      area = unique(area),
      mean_cpue = mean(cpue_mt_km2),
      var_cpue = var(cpue_mt_km2),
      est = area * mean_cpue,
      var = var_cpue * (area * area) / ntows
    ) |>
    dplyr::ungroup()

  biomass_by_year <- biomass_year_stratum |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      mean_est = sum(est),
      se = sqrt(sum(var)),
      cv = sqrt(sum(var)) / sum(est),
      log_var = log(cv^2 + 1),
      est = mean_est * exp(-0.5 * log_var),
      se_log = sqrt(log_var),
      lwr = exp((log(est) - qnorm(1 - (1 - CI) / 2) * se_log)),
      upr = exp((log(est) + qnorm(1 - (1 - CI) / 2) * se_log))
    ) |>
    dplyr::ungroup()

  biomass <- biomass_by_year[, c("year", "est", "se_log", "lwr", "upr")]

  biomass_estimates <- list(
    biomass_by_strata = as.data.frame(biomass_year_stratum),
    biomass = as.data.frame(biomass))

  if (!is.null(dir)) {
    biomass_out <- data.frame(
      year = biomass[, "year"],
      month = month,
      fleet = fleet,
      est = biomass[, "est"],
      se_log = biomass[, "se_log"]
    )
    write.csv(
      biomass_out,
      file = file.path(plotdir, paste("design_based_indices.csv", sep = "")),
      row.names = FALSE)
  }

  return(biomass_estimates)
}
