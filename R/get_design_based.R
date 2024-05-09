#' Calculates design based estimates from survey data for West Coast surveys.
#'
#' @details
#' The design based index is calculated based on the area of the strata and
#' the mean catch by strata. This function returns a list of design-based
#' estimates by strata and estimates combined across stratas by year. This
#' function is designed to work with data frames pulled from the NWFSC
#' data warehouse using [pull_catch()].
#' See: Gunderson, D.R. and Sample, T.M. 1980. Distribution and abundance of rockfish off Washington,
#' Oregon, and California during 1977. Marine Fisheries Review: March - April.
#'
#' @param catch Data frame of catch data that has been created by the [pull_catch()].
#' @template dir
#' @template strat_vars
#' @template strata
#' @template printfolder
#' @template month
#' @template fleet
#' @template verbose
#'
#' @returns List of biomass estimates by year and biomass estimates by year and
#' strata.
#'
#' @author Allan Hicks and Chantel Wetzel
#' @importFrom grDevices dev.off png rgb
#' @importFrom graphics axis legend mtext par plot points segments symbols
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
#'   catch = catch,
#'   strata = strata
#' )
#'
#' }
#'
get_design_based <- function(
  catch,
  strata,
  strata_vars = c("Depth_m", "Latitude_dd"),
  dir = NULL,
  month = NA,
  fleet = NA,
  printfolder = "forSS3",
  verbose = TRUE)
{

  plotdir <- file.path(dir, printfolder)
  check_dir(dir = plotdir, verbose = verbose)

  original_colnames <- colnames(catch)
  colnames(catch) <- tolower(colnames(catch))
  strata_vars <- tolower(strata_vars)
  colnames(strata) <- tolower(colnames(strata))

  if (sum(strata_vars %in% colnames(catch)) != length(strata_vars)) {
    stop("The stata_vars are not found in the catch data frame.")
  }

  if (nrow(strata) == 1) {
    warning("It is recommended to have more than one strata, consider revising strata.")
  }

  if (is.null(catch[, "cpue_kg_km2"])) {
    stop("There must be a column called cpue_kg_km2 in the catch data frame")
  }

  # Calculate the CPUE in terms of numbers
  catch[, "cpue_km2_count"] <- catch[, "total_catch_numbers"] / (0.01 * catch[, "area_swept_ha"])

  # Add rownames to make it easier to index later
  row.names(strata) <- strata[, 1]
  n_strata <- nrow(strata)

  # create strata factors
  stratum <- rep(NA, nrow(catch)) # the stratum factor
  for (n in 1:n_strata) {
    ind <- rep(TRUE, nrow(catch))
    for (s in 1:length(strata_vars)) {
      ind <- ind &
        catch[, strata_vars[s]] >= strata[n, paste(strata_vars[s], ".1", sep = "")] &
        catch[, strata_vars[s]] <  strata[n, paste(strata_vars[s], ".2", sep = "")]
    }
    stratum[ind] <- as.character(strata[n, 1])
  }
  #stratum <- factor(stratum, levels = as.character(strata[, 1]))
  catch <- data.frame(catch, stratum)

  # add area for each stratum to data frame
  colnames(strata)[1] <- "stratum"
  catch_filtered <- catch |>
    dplyr::filter(!is.na(stratum))
  catch_mod <- dplyr::left_join(catch_filtered, strata[, 1:2])

  biomass_year_stratum <- catch_mod |>
    dplyr::group_by(year, stratum) |>
    dplyr::summarize(
      ntows = n(),
      area = unique(area),
      mean_cpue = mean(cpue_kg_km2),
      var_cpue = var(cpue_kg_km2),
      mean_cpue_area = area * mean_cpue,
      var_cpue_area = var_cpue * (area * area) / ntows,
      cv = sqrt(var_cpue_area) / (mean_cpue_area + 0.000000001),
      log_var = sqrt(log(cv^2 + 1))
    ) |>
    ungroup()

  if (any(biomass_year_stratum[, "ntows"] <= 1)) {
    bad_strata <- biomass_year_stratum[which(biomass_year_stratum[, "ntows"] <= 1), c("year", "stratum")]
    warning(bad_strata)
    stop("The above year-strata combinations have one or less observations.
         \n The stratas will need to be revised.")
  }

  biomass_by_year <- biomass_year_stratum |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      mean_bhat = sum(mean_cpue_area),
      se = sqrt(sum(var_cpue_area)),
      cv = sqrt(sum(var_cpue_area)) / sum(mean_cpue_area),
      log_var = log(cv^2 + 1),
      month = month,
      fleet = fleet,
      median_bhat = mean_bhat * exp(-0.5 * log_var),
      se_log = sqrt(log_var)
    ) |>
    ungroup()

  biomass <- biomass_by_year[, c("year", "month", "fleet", "median_bhat", "se_log")]

  biomass_estimates <- list(
    biomass_by_strata = as.data.frame(biomass_year_stratum),
    biomass = as.data.frame(biomass))

  if (!is.null(dir)) {
    write.csv(
      biomass,
      file = file.path(plotdir, paste("design_based_indices.csv", sep = "")),
      row.names = FALSE)
  }

  return(biomass_estimates)
}
