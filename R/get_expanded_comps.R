#' Calculate expanded composition data for lengths and marginal ages
#'
#' Conduct a two-stage expansion of length and age composition data. The first
#' stage expands data up to the tow level and the second stage expands the data
#' up the user-defined strata areas. This function is designed to be used with
#' catch pulled using [pull_catch()] and biological data pulled using [pull_bio()].
#'
#' @param bio_data A data frame of length-composition data returned from
#'   [pull_bio()].
#' @param catch_data A data frame of catch data returned from [pull_catch()].
#' @param comp_bins Vector of intergers to bin length or age data by
#'   create expanded composition data.Values above or below the minimum or maximum
#'   values in the vector are grouped into the first size or plus group size, respectively.
#'   For example, creating length compositions that uses a vector bin of seq(10, 50, 2)
#'   would create 2 cm bins where fish length between [0, 11.99) would be included in the
#'   10 cm bin, fish of length [12, 13.99) would be included in the 12 cm bin, and
#'   all fish [50- Inf) would be included in the 50 cm plus bin.
#' @template strata
#' @template dir
#' @param comp_column_name The name of the column to create composition data for that
#'   must be a string. This column can be is used to determine whether to format the
#'   composition data for length or age compositions by looking for either age
#'   (e.g., `age_years`, `Age`, `age`, `best_age`) or length (e.g., `length`,
#'   `length_cm`, `Length`, `Length_cm`) in the comp_column_name. The default is `length_cm`.
#' @param output Switch to specify how to return the composition data where the options
#'   are c("full_expansion_ss3_format", "tow_expansion_only", "full_expansion_unformatted").
#'   The default is `output = "full_expansion_ss3_format"` where a list is returned with
#'   formatted composition data for SS3. The `tow_expansion_only` retuns a dataframe of
#'   composition data only expanded up to the tow level (first-stage) and
#'   `full_expansion_unformatted` returns a dataframe with compositon data expanded up to
#'   the tow and strata level but not formatted for SS3.
#' @param two_sex_comps Default TRUE. If TRUE composition data will be formatted for a
#'   Stock Synthesis two-sex model and if FALSE composition data will be formatted for a
#'   single-sex model.
#' @param input_n_method Determines the default input sample size to add to
#'   the composition data for SS3. There are three options: c("stewart_hamel", "tows",
#'   "total_samples") where the default is "stewart_hamel".
#' @template partition
#' @param age_low Lower age bin for all age composition data based on the expected
#'   format for Stock Synthesis. Default value of -1 which translates to the lowest age
#'   bin.
#' @param age_high Upper age bin for all age composition data based on the expected
#'   format for Stock Synthesis. Default value of -1 which translates to the highest
#    age bin.
#' @param age_error Number of ageing error vector to apply to the age data based on
#'   Stock Synthesis. Default "Enter Age Error Vector".
#' @param fleet A fleet number to assign the composition data to based on the expected
#'   format for Stock Synthesis. Default "Enter Fleet".
#' @param month Month the samples were collected based on the expected format for
#'   Stock Synthesis to determine the length/age estimate to compare to. Default
#'   "Enter Month".
#' @template printfolder
#' @template verbose
#'
#' @seealso See
#' \code{\link{get_input_n}} for information on input sample size calculations.
#'
#' @author Chantel Wetzel and Allan Hicks
#' @return A list or dataframe is returned depending upon `output`.
#' The default `output = "full_expansion_ss3_format"` returns a list of
#' expanded composition data by sex grouping (e.g., sexed and unsexed fish)
#' formatted for Stock Synthesis v.3.30+. If `output = "full_expansion_unformatted"`
#' a dataframe is returned of unformatted expanded composition data and if
#' `output = "tow_expansion_only"` a dataframe is returned with the composition
#' data only expanded to the tow level (first stage expansion only).
#' @export
#'
#' @examples
#' \dontrun{
#' bio <- pull_bio(
#'   common_name = "lingcod",
#'   survey = "NWFSC.Combo"
#' )
#'
#' catch <- pull_catch(
#'   common_name = "lingcod",
#'   survey = "NWFSC.Combo"
#' )
#'
#' strata <-  CreateStrataDF.fn(
#'   names = c("shallow_wa", "shallow_or", "shallow_nca", "shelf_wa", "shelf_or", "shelf_nca"),
#'   depths.shallow = c( 55,   55,    55,  183,  183,  183),
#'   depths.deep    = c(183,  183,   183,  350,  350,  350),
#'   lats.south     = c(46.0, 42.0, 40.10, 46.0, 42.0, 40.10),
#'   lats.north     = c(49.0, 46.0, 42.0,  49.0, 46.0, 42.0)
#' )
#'
#' length_comps <- get_expanded_comps(
#'   bio_data = bio,
#'   catch_data = catch,
#'   strata = strata,
#'   comp_bins = seq(20, 70, 4),
#'   comp_column_name = "length_cm"
#' )
#'
#' }
#'
get_expanded_comps <- function(
    bio_data,
    catch_data,
    comp_bins,
    strata,
    dir = NULL,
    comp_column_name = "length_cm",
    output = c("full_expansion_ss3_format", "tow_expansion_only", "full_expansion_unformatted"),
    two_sex_comps = TRUE,
    input_n_method = c("stewart_hamel", "tows", "total_samples"),
    partition = 0,
    fleet = "Enter Fleet",
    age_low = -1,
    age_high = -1,
    age_error = "Enter Numeric",
    month = "Enter Month",
    printfolder = "forSS3",
    verbose = TRUE) {

  plotdir <- file.path(dir, printfolder)
  check_dir(dir = dir, verbose = verbose)

  input_n_method <- rlang::arg_match0(input_n_method, input_n_method)
  output <- rlang::arg_match0(output, output)

  # Convert all the column names to lower case so that code works with old and
  # data pull formats
  colnames(bio_data) <- tolower(colnames(bio_data))
  colnames(catch_data) <- tolower(colnames(catch_data))
  colnames(strata) <- tolower(colnames(strata))
  # Check for needed columns
  required_bio_columns <- c(
    comp_column_name,
    "sex",
    "year",
    "trawl_id",
    "depth_m",
    "latitude_dd",
    "common_name",
    "project"
  )
  required_catch_columns <- c(
    "year",
    "trawl_id",
    "depth_m",
    "latitude_dd",
    "area_swept_ha",
    "total_catch_numbers"
  )
  if (any(!required_bio_columns %in% colnames(bio_data))) {
    missing_columns <- required_bio_columns[!required_bio_columns %in% colnames(bio_data)]
    cli::cli_abort(
      "The following column(s) are missing in the bio_data: {missing_columns}"
    )
  }
  if (any(!required_catch_columns %in% colnames(catch_data))) {
    missing_columns <- required_catch_columns[!required_catch_columns %in% colnames(catch_data)]
    cli::cli_abort(
      "The following column(s) are missing in the catch_data: {missing_columns}"
    )
  }
  # Put in row names to make easier to index later
  row.names(strata) <- strata[, 1]
  comp_column_name <- tolower(comp_column_name)

  strata_vars <- c("depth_m", "latitude_dd")
  check_strata <- tolower(unique(gsub("\\..*", "", colnames(strata))))
  if (any(!c("depth_m", "latitude_dd") %in% check_strata)) {
    cli::cli_abort(
      "The strata needs to be by depth_m and latitude_dd."
    )
  }
    bio_data[, "sex"] <- "U"
  }

  strata_vars = c("depth_m", "latitude_dd")
  bio_data[, "comp_column"] <- bio_data[, comp_column_name]

  bio_data <- bio_data |>
    dplyr::filter(
      !is.na(comp_column)) |>
    dplyr::mutate(
      sex = ifelse(is.na(sex), "U", sex)
    )

  bins <- c(-999, comp_bins, Inf)
  bio_data[, "bin"] <- bins[findInterval(as.numeric(bio_data[, "comp_column"]), bins, all.inside = T)]
  check <- sum(bio_data[, "bin"] == -999) / dim(bio_data)[1]
  if (verbose) {
    percent_min <- round(100 * sum(bio_data[, "bin"] == -999) / dim(bio_data)[1], 2)
    percent_max <- round(100 * sum(bio_data[, "comp_column"] >= max(comp_bins)) / dim(bio_data)[1], 2)
    cli::cli_bullets(c(
     i = "There are {percent_min}% of records that are less than the minimum
     composition bin. These fish will be added to the minimum bin.",
     i = "There are {percent_max}% of records that are greater than the maximum
     composition bin. These fish will be added to the maximum bin.")
    )
  }
  bio_data[which(bio_data$bin == -999), "bin"] <- min(comp_bins)

  positive_tows <- catch_data[which(catch_data$total_catch_numbers > 0), ]
  find <- !(positive_tows$trawl_id %in% bio_data$trawl_id)
  no_samples_taken <- sum(find)
  missing <- sum(positive_tows[find, "total_catch_numbers"])
  percent <- 100 * round(missing / sum(catch_data[, "total_catch_numbers"]), 3)
  if (verbose) {
    cli::cli_alert_info("There are {no_samples_taken} tows where fish were observed but not sampled.
        These tows comprise {percent} percent of the total catch numbers.
        Only measured fished in the bio_data file are used for composition expansions"
    )
  }

  bio_data <- data.frame(
    bio_data,
    strata = StrataFactors.fn(bio_data, strata_vars, strata)
  )
  bio_data <- bio_data |> dplyr::filter(!is.na(strata))

  catch_data[, "area_swept"] <- catch_data[, "area_swept_ha"] * 0.01

  catch_data <- data.frame(
    catch_data,
    strata = StrataFactors.fn(catch_data, strata_vars, strata)
  )
  strata[, "strata"] <- strata[, "name"]
  catch_data <- dplyr::left_join(
    catch_data,
    strata[, c("strata", "area")],
    by = "strata"
  )

  catch_data <- catch_data |>
    dplyr::group_by(year, strata) |>
    dplyr::mutate(
      tows = n()
    )

  bio_data <- bio_data |>
    dplyr::group_by(trawl_id) |>
    dplyr::mutate(
      all_fish = n()
    )

  bio_catch <- dplyr::left_join(
    bio_data[, c("year", "trawl_id", "comp_column", "sex", "bin", "all_fish")],
    catch_data[, c("trawl_id", "area_swept", "strata", "area", "tows", "total_catch_numbers")],
    by = "trawl_id") |>
    dplyr::filter(!is.na(strata)) |>
    dplyr::group_by(trawl_id, comp_column) |>
    dplyr::summarize(
      year = unique(year),
      area_swept =  unique(area_swept),
      strata =  unique(strata),
      strata_area = unique(area),
      tows = unique(tows),
      total_catch_numbers = unique(total_catch_numbers),
      bin = unique(bin),
      all_fish = unique(all_fish),
      n_all = n(),
      n_female = sum(sex == "F"),
      n_male = sum(sex == "M"),
      n_unsexed = sum(sex == "U"),
      multiplier = total_catch_numbers / all_fish,
      exp_all = n_all * multiplier,
      exp_f = n_female * multiplier,
      exp_m = n_male * multiplier,
      exp_u = n_unsexed * multiplier
      )

  if (output == "tow_expansion_only") {
    if (verbose) {
      cli::cli_alert_info(
        "Composition data only expanded to the tow level.
        Formatted composition data file not written for SS3."
      )
    }
    tow_expansion_data <- bio_catch
    return(tow_expansion_data)
  }

  stratum_exp <- bio_catch |>
    dplyr::group_by(year, strata, bin) |>
    dplyr::reframe(
      all = unique(strata_area) * sum(exp_all / area_swept) / unique(tows),
      female = unique(strata_area) * sum(exp_f / area_swept) / unique(tows),
      male = unique(strata_area) * sum(exp_m / area_swept) / unique(tows),
      unsexed = unique(strata_area) * sum(exp_u / area_swept) / unique(tows),
    )

  total_by_year <- stratum_exp |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      prop_total_all = sum(all),
      prop_total_fm = sum(female + male),
      prop_total_unsexed = sum(unsexed)
    )

  comps_by_year <- total_by_year |>
    dplyr::group_by(year, bin) |>
    dplyr::summarize(
      total_all = sum(all),
      total_female = sum(female),
      total_male = sum(male),
      total_unsexed = sum(unsexed),
      prop_all = 100 * total_all / unique(prop_total_all),
      prop_female = 100 * total_female / unique(prop_total_fm),
      prop_male = 100 * total_male / unique(prop_total_fm),
      prop_unsexed = 100 * total_unsexed / unique(prop_total_unsexed)
    ) |>
    dplyr::ungroup()

  comps_by_year <- comps_by_year |>
    tidyr::complete(year, bin,
      fill = list(
        total_all = 0,
        total_female = 0,
        total_male = 0,
        total_unsexed = 0,
        prop_all = 0,
        prop_female = 0,
        prop_male = 0,
        prop_unsexed = 0))

  if (output == "full_expansion_unformatted") {
    return(comps_by_year)
  }

  lgths <- as.character(unique(comps_by_year$bin))
  # otherwise return SS3 output for gender type
  all_comps <- comps_by_year[, c("year", "bin", "prop_all")] |>
    tidyr::pivot_wider(
    names_from = bin,
    names_prefix = "U",
    values_from = prop_all)
  all_comps[is.na(all_comps)] <- 0

  unsexed_comps <- comps_by_year[, c("year", "bin", "prop_unsexed")] |>
    tidyr::pivot_wider(
      names_from = bin,
      names_prefix = "U",
      values_from = prop_unsexed)
  unsexed_comps[is.na(unsexed_comps)] <- 0

  female_comps <- comps_by_year[, c("year", "bin", "prop_female")] |>
    tidyr::pivot_wider(
      names_from = bin,
      names_prefix = "F",
      values_from = prop_female)
  female_comps[is.na(female_comps)] <- 0

  male_comps <- comps_by_year[, c("year", "bin", "prop_male")] |>
    tidyr::pivot_wider(
      names_from = bin,
      names_prefix = "M",
      values_from = prop_male)
  male_comps[is.na(male_comps)] <- 0

  # Calculate input sample size based on existing function
  species <- gsub(" ", "_", tolower(unique(bio_data[, "common_name"])))
  species_type <- get_species_info(
    species = species,
    unident = FALSE,
    verbose = FALSE
  )$species_type

  samples <- get_input_n(
    dir = dir,
    data = bio_data,
    comp_column_name = comp_column_name,
    input_n_method = input_n_method,
    species_group = species_type,
    printfolder = printfolder,
    verbose = verbose)

  dimensions <- 2:ncol(all_comps)
  project <- gsub(" ", "_", tolower(unique(bio_data[, "project"])))
  bin_range <- paste0(min(comp_bins), "_", max(comp_bins))

  comps <- list()
  if (two_sex_comps) {
    sexed_formatted <- data.frame(
      year = female_comps[, "year"],
      month = month,
      fleet = fleet,
      sex = 3,
      partition = partition,
      input_n = samples |> dplyr::filter(sex_grouped == "sexed") |> dplyr::select(input_n)
    )
    sexed_formatted <- cbind(sexed_formatted, female_comps[,dimensions], male_comps[, dimensions])
    remove <- which(apply(sexed_formatted[, 7:ncol(sexed_formatted)], 1, sum) == 0)
    if(length(remove) > 0) {
      sexed_formatted <- sexed_formatted[-remove, ]
    }

    unsexed_formatted <- data.frame(
      year = unsexed_comps[, "year"],
      month = month,
      fleet = fleet,
      sex = 0,
      partition = partition,
      input_n = samples |> dplyr::filter(sex_grouped == "unsexed") |> dplyr::select(input_n)
    )
    unsexed_formatted <- cbind(unsexed_formatted, unsexed_comps[, dimensions], 0 * unsexed_comps[, dimensions])
    remove <- which(apply(unsexed_formatted[, 7:ncol(unsexed_formatted)], 1, sum) == 0)
    if(length(remove) > 0) {
      unsexed_formatted <- unsexed_formatted[-remove, ]
    }

    if (length(grep("age", comp_column_name, ignore.case = TRUE)) > 0) {
      sexed_formatted <- cbind(sexed_formatted[, 1:5], age_error, age_low, age_high, sexed_formatted[, 6:dim(sexed_formatted)[2]])
      if (dim(unsexed_formatted)[1] > 0) {
        unsexed_formatted <- cbind(unsexed_formatted[, 1:5], age_error, age_low, age_high, unsexed_formatted[, 6:dim(unsexed_formatted)[2]])
      }
    }

    if (!is.null(dir)) {
      write.csv(
        x = sexed_formatted,
        file = file.path(dir, printfolder, paste0(comp_column_name, "_sexed_expanded_", bin_range, "_", species, "_", project, ".csv")),
        row.names = FALSE
      )
      if (dim(unsexed_formatted)[1] > 0) {
        write.csv(
          x = unsexed_formatted,
          file = file.path(dir, printfolder, paste0(comp_column_name, "_unsexed_expanded_", bin_range, "_", species, "_", project, ".csv")),
          row.names = FALSE
        )
      }
    }
    comps$sexed <- sexed_formatted
    comps$unsexed <- unsexed_formatted
  } else {
    all_formatted <- data.frame(
      year = all_comps[, "year"],
      month = month,
      fleet = fleet,
      sex = 0,
      partition = partition,
      input_n = samples |> dplyr::filter(sex_grouped == "all") |> dplyr::select(input_n)
    )
    all_formatted <- cbind(all_formatted, all_comps[, dimensions])
    remove <- which(apply(all_formatted[, 7:ncol(all_formatted)], 1, sum) == 0)
    if(length(remove) > 0) {
      all_formatted <- all_formatted[-remove, ]
    }

    if (grep("age", comp_column_name, ignore.case = TRUE) > 0) {
      all_formatted <- cbind(all_formatted[, 1:5], age_error, age_low, age_high, all_formatted[, 6:dim(all_formatted)[2]])
    }

    if (!is.null(dir)) {
      write.csv(
        x = all_formatted,
        file = file.path(dir, printfolder, paste0(comp_column_name, "_unsexed_expanded_", bin_range, "_", ,species,, "_", project, ".csv")),
        row.names = FALSE
      )
    }

    comps$unsexed <- all_formatted
  }


  return(comps)
}
