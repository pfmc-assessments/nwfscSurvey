#' Calculate unexpanded/raw length or marginal age compositions
#'
#' @details
#' Creates a matrix of unexpanded (or raw) marginal length or age composition
#' data formatted for Stock Synthesis. The code will return composition data
#' for all sexes present in the data frame and no sex assignment is done for
#' unsexed fish. The function will create composition data for either
#' lengths or ages based on the comp_column_name. The function will return a
#' list of composition data based upon the sexes present in the data for a
#' two-sex model or all length/ages for single-sex model.
#'
#' @inheritParams get_expanded_comps
#' @param data A data frame that includes columns of year, sex, and length/ages. The data
#'   frame can be survey data pulled using pull_bio from the data warehouse or any data frame
#'   that includes column names of sex, year, and the comp_column_name.  The sex column is
#'   expected to have sexes denoted by F, M, and U.
#' @param comp_column_name The column name to create composition data for. This column can be
#'   is used to determine whether to format the composition data for length or age
#'   compositions by looking for either age (e.g., `age_years`, `Age`, `best_age`) or length
#'   (e.g., `Length`, `length`, `Length_cm`) in the comp_column_name. The default
#'   is `Length_cm`.
#'
#' @returns A list of length or marginal age compositions for sexed and
#' unsexed fish formatted for Stock Synthesis.
#'
#' @author Chantel Wetzel
#' @export
#'
#' @examples
#' \dontrun{
#' bio <- pull_bio(
#'   common_name = "lingcod",
#'   survey = "NWFSC.Combo"
#' )
#'
#' length_comps <- get_raw_comps(
#'   data = bio,
#'   comp_bins = seq(20, 70, 4)
#' )
#'
#' age_comps <- get_raw_comps(
#'   data = bio,
#'   comp_bins = 1:20,
#'   comp_column_name = "Age"
#' )
#' }
#'
get_raw_comps <- function(
    data,
    comp_bins,
    comp_column_name = "Length_cm",
    input_n_method = c("stewart_hamel", "tows", "total_samples"),
    two_sex_comps = TRUE,
    month = "Enter Month",
    fleet = "Enter Fleet",
    partition = 0,
    ageerr = "Enter Numeric",
    Lbin_lo = -1,
    Lbin_hi = -1,
    age_low = lifecycle::deprecated(),
    age_high = lifecycle::deprecated(),
    age_error = lifecycle::deprecated(),
    dir = NULL,
    printfolder = "forSS3",
    verbose = TRUE) {
  # arguments deprecated to be consistent with output column names
  # revised to better match r4ss
  # https://github.com/pfmc-assessments/nwfscSurvey/issues/164
  if (lifecycle::is_present(age_low)) {
    lifecycle::deprecate_warn(
      when = "2.2",
      what = "nwfscSurvey::get_expanded_comps(age_low =)",
      with = "nwfscSurvey::get_expanded_comps(Lbin_lo =)"
    )
  }
  if (lifecycle::is_present(age_high)) {
    lifecycle::deprecate_warn(
      when = "2.2",
      what = "nwfscSurvey::get_expanded_comps(age_high =)",
      with = "nwfscSurvey::get_expanded_comps(Lbin_hi =)"
    )
  }
  if (lifecycle::is_present(age_error)) {
    lifecycle::deprecate_warn(
      when = "2.2",
      what = "nwfscSurvey::get_expanded_comps(age_error =)",
      with = "nwfscSurvey::get_expanded_comps(ageerr =)"
    )
  }

  plotdir <- file.path(dir, printfolder)
  check_dir(dir = plotdir, verbose = verbose)

  input_n_method <- rlang::arg_match(input_n_method)

  colnames(data) <- tolower(colnames(data))
  comp_column_name <- tolower(comp_column_name)

  vars <- c("year", "sex")
  if (sum(vars %in% colnames(data)) != 2) {
    cli::cli_abort(
      "Data frame does not contain a column name year and/or sex.
      The columns names can be either upper or lower case."
    )
  }

  if (!comp_column_name %in% colnames(data)) {
    cli::cli_abort(
      "Data frame does not contain a column name of comp_column_name.
      The columns names can be either upper or lower case."
    )
  }

  if (!two_sex_comps) {
    data[, "sex"] <- "U"
  }

  # Check to see if user is doing ages or lengths
  if (length(grep("age", comp_column_name)) > 0) {
    comp_type <- "age"
  } else {
    comp_type <- "length"
  }

  keep <- !is.na(data[, comp_column_name])
  data <- data[keep, ]
  bins <- c(comp_bins, Inf)
  data$bin <- bins[findInterval(data[, comp_column_name], bins, all.inside = T)]

  # if there are NA sexes replace them with U
  if (sum(is.na(data[, "sex"])) > 0) {
    data[is.na(data[, "sex"]), "sex"] <- "U"
  }

  if (!"common_name" %in% colnames(data) & input_n_method == "stewart_hamel") {
    cli::cli_abort(
      "Data frame does not contain a column name of common_name which is required
      to calculate Stewart and Hamel input sample size. The columns names can be
      either upper or lower case."
    )
  }

  # Calculate input sample size based on existing function
  species <- ifelse("common_name" %in% colnames(data), unique(data[, "common_name"]), "")
  if ("common_name" %in% colnames(data)) {
    species_type <- get_species_info(
      species = species,
      unident = FALSE,
      verbose = FALSE
    )$species_type
  } else {
    species_type <- "all"
  }

  if (!"trawl_id" %in% colnames(data)) {
    data[, "trawl_id"] <- 1:nrow(data)
  }

  samples <- get_input_n(
    dir = dir,
    data = data,
    comp_column_name = comp_column_name,
    input_n_method = input_n_method,
    species_group = species_type,
    printfolder = printfolder,
    verbose = verbose
  )

  # Create the comps
  Results <- out <- NULL
  for (y in sort(unique(data[, "year"]))) {
    # Identify relevant rows
    Which <- which(data[, "year"] == y & data[, "sex"] %in% c("F", "M"))
    # Skip this year unless there are rows
    if (length(Which) > 0) {
      Row <- c(y, length(Which))
      # Loop across F then M
      for (s in c("F", "M")) {
        # Loop across length bins
        for (l in comp_bins)
        {
          # Subset to relevant rows
          if (l == min(bins)) {
            Which2 <- Which[which(data[Which, "bin"] %in% l & data[Which, "sex"] == s)]
          }
          if (l != min(bins)) {
            Which2 <- Which[which(data[Which, "bin"] == l & data[Which, "sex"] == s)]
          }
          if (l == max(bins)) {
            Which2 <- Which[which(data[Which, "bin"] %in% c(l, Inf) & data[Which, "sex"] == s)]
          }
          # Sum to effective sample size by length_bin x Sex x Fleet x Year
          if (length(Which2) == 0) Row <- c(Row, 0)
          if (length(Which2) >= 1) Row <- c(Row, length(Which2))
        }
      }
      # Add to results matrix
      Results <- rbind(Results, Row)
    } # end Which loop
  } # end year loop

  if (!is.null(Results)) {
    Results <- as.data.frame(Results)
    tmp <- data.frame(
      year = Results[, 1],
      month = month,
      fleet = fleet,
      sex = 3,
      partition = partition,
      input_n = samples |> dplyr::filter(sex_grouped == "sexed") |> dplyr::select(input_n) # Results[, 2]
    )
    out <- cbind(tmp, Results[, -c(1:2)])
    colnames(out)[-c(1:6)] <- c(
      paste(rep("f", each = length(comp_bins)), comp_bins, sep = ""),
      paste(rep("m", each = length(comp_bins)), comp_bins, sep = "")
    )
  }

  # Create unsexed comps if present in the data
  out_u <- NULL
  if (length(data[data[, "sex"] == "U", "sex"]) > 0) {
    Results <- NULL
    for (y in sort(unique(data[, "year"]))) {
      # Identify relevant rows
      Which <- which(data[, "year"] == y & data[, "sex"] == "U")
      # Skip this year unless there are rows
      if (length(Which) > 0) {
        # Format reference stuff
        Row <- c(y, length(Which))
        # Loop across length bins
        for (l in comp_bins)
        {
          # Subset to relevant rows
          if (l == min(bins)) {
            Which2 <- Which[which(data[Which, "bin"] %in% l)]
          }
          if (l != min(bins)) {
            Which2 <- Which[which(data[Which, "bin"] == l)]
          }
          if (l == max(bins)) {
            Which2 <- Which[which(data[Which, "bin"] %in% c(l, Inf))]
          }
          # Sum to effective sample size by length_bin x Sex x Fleet x Year
          if (length(Which2) == 0) Row <- c(Row, 0)
          if (length(Which2) >= 1) Row <- c(Row, length(Which2))
        }
        # Add to results matrix
        Results <- rbind(Results, Row)
      } # end Which loop
    } # end year loop
    Results <- as.data.frame(Results)
    if (sum(c("M", "F") %in% data[, "sex"]) == 0) {
      input_n <- samples |>
        dplyr::filter(sex_grouped == "all", input_n != 0) |>
        dplyr::select(input_n)
    } else {
      input_n <- samples |>
        dplyr::filter(sex_grouped == "unsexed", , input_n != 0) |>
        dplyr::select(input_n)
    }

    tmp <- data.frame(
      year = Results[, 1],
      month = month,
      fleet = fleet,
      sex = 0,
      partition = partition,
      input_n = input_n # Results[, 2]
    )

    if (two_sex_comps) {
      out_u <- cbind(tmp, Results[, -c(1:2)], 0 * Results[, -c(1:2)])
    } else {
      out_u <- cbind(tmp, Results[, -c(1:2)])
    }
    colnames(out_u)[-c(1:6)] <- paste(rep("u", each = length(comp_bins)), comp_bins, sep = "")
  }

  if (comp_type == "length") {
    if (!is.null(out)) {
      out_comps <- out
    } else {
      out_comps <- NULL
    }
    if (!is.null(out_u)) {
      out_u_comps <- out_u
    }
  }

  if (comp_type == "age") {
    if (!is.null(out)) {
      out_comps <- cbind(out[, 1:5], ageerr, Lbin_lo, Lbin_hi, out[, 6:dim(out)[2]])
    } else {
      out_comps <- NULL
    }
    if (!is.null(out_u)) {
      out_u_comps <- cbind(out_u[, 1:5], ageerr, Lbin_lo, Lbin_hi, out_u[, 6:dim(out_u)[2]])
    }
  }

  if (!is.null(dir)) {
    project <- ifelse("project" %in% colnames(data),
      gsub(" ", "_", tolower(unique(data[, "project"]))),
      ""
    )
    bin_range <- paste0(min(comp_bins), "_", max(comp_bins))
    if (!is.null(out_comps)) {
      write.csv(out_comps,
        file = file.path(plotdir, paste0(comp_column_name, "_sexed_raw_", bin_range, "_", species, "_", project, ".csv")),
        row.names = FALSE
      )
    }
    if (!is.null(out_u)) {
      write.csv(out_u_comps,
        file = file.path(plotdir, paste0(comp_column_name, "_unsexed_raw_", bin_range, "_", species, "_", project, ".csv")),
        row.names = FALSE
      )
    }
  }

  comps <- list()
  if (!is.null(out_comps)) {
    rownames(out_comps) <- NULL
    comps$sexed <- out_comps
  }
  if (!is.null(out_u)) {
    rownames(out_u_comps) <- NULL
    comps$unsexed <- out_u_comps
  }

  return(comps)
}
