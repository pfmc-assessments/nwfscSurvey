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
#' @param len_bins Vector of integers to bin length data by
#'   create expanded composition data. Values above or below the minimum or maximum
#'   values in the vector are grouped into the first size or plus group size, respectively.
#'   For example, creating length compositions that uses a vector bin of seq(10, 50, 2)
#'   would create 2 cm bins where fish length between [0, 11.99) would be included in the
#'   10 cm bin, fish of length [12, 13.99) would be included in the 12 cm bin, and
#'   all fish [50- Inf) would be included in the 50 cm plus bin.
#' @param age_bins Vector of integers to bin age data by
#'   create expanded composition data. Values above or below the minimum or maximum
#'   values in the vector are grouped into the first size or plus group size, respectively.
#'   For example, creating age compositions that uses a vector bin of seq(1, 50, 1)
#'   would create 1 year age bins and all ages [50-Inf) will be in the plus group
#'   age bin.
#' @param length_column_name The length column name to create conditional age-at-length
#'  data for. The default is `length_cm`.
#' @param age_column_name The age column name to create conditional age-at-length
#'  data for. The default is `age`.
#'
#' @returns A data frame of conditiona age-at-length compositions for sexed and
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
#' caal_data <- get_raw_caal(
#'   data = bio,
#'   len_bins = seq(20, 70, 4),
#'   age_bins = 1:30,
#' )
#' }
#'
get_raw_caal <- function(
    data,
    len_bins,
    age_bins,
    length_column_name = "length_cm",
    age_column_name = "age",
    dir = NULL,
    month = "Enter Month",
    fleet = "Enter Fleet",
    partition = 0,
    ageerr = "Enter Numeric",
    printfolder = "forSS3",
    verbose = TRUE) {
  plotdir <- file.path(dir, printfolder)
  check_dir(dir = plotdir, verbose = verbose)

  colnames(data) <- tolower(colnames(data))
  length_column_name <- tolower(length_column_name)
  age_column_name <- tolower(age_column_name)

  vars <- c("year", "sex", length_column_name, age_column_name)
  if (sum(vars %in% colnames(data)) != 4) {
    cli::cli_abort(
      "Data frame does not contain a column name {length_column_name}, {age_column_name}, year and/or sex.
      The columns names can be either upper or lower case."
    )
  }

  data[, "len_col"] <- data[, colnames(data) == length_column_name]
  data[, "age_col"] <- data[, colnames(data) == age_column_name]

  data <- data |>
    dplyr::filter(!is.na(len_col), !is.na(age_col))
  data[, "sex"] <- codify_sex(data[, "sex"])

  ls <- c(-999, len_bins, Inf)
  as <- c(-999, age_bins, Inf)

  # Start matrix to save results
  data[, "allLs"] <- ls[findInterval(data[, "len_col"], ls, all.inside = TRUE)]
  data[, "allAs"] <- as[findInterval(data[, "age_col"], as, all.inside = TRUE)]

  sex_loop <- unique(data[, "sex"])
  year_loop <- unique(data[, "year"])

  comps_df <- comps_row <- year <- sex <- input_n <- lbin_low <- NULL
  # Loop across F then M
  for (s in sex_loop) {
    # Loop across years
    year_loop <- sort(unique(data[, "year"][data[, "sex"] == s]))
    for (y in year_loop) {
      ########## CONDITIONAL
      # Loop across Length-bins
      for (l in len_bins) {
        # Identify relevant rows
        if (l == min(len_bins)) {
          find <- which(data[, "sex"] == s & data[, "year"] == y & data[, "allLs"] %in% c(-999, l))
        }
        if (l == max(len_bins)) {
          find <- which(data[, "sex"] == s & data[, "year"] == y & data[, "allLs"] %in% c(Inf, l))
        }
        if (!l %in% c(min(len_bins), max(len_bins))) {
          find <- which(data[, "sex"] == s & data[, "year"] == y & data[, "allLs"] == l)
        }
        # Skip this year unless there are rows
        if (length(find) > 0) {
          # Loop across age bins
          comps_row <- NULL
          for (a in age_bins) {
            # Subset to relevant rows
            if (a == min(age_bins)) {
              find2 <- find[which(data[find, "allAs"] %in% c(-999, a))]
            }
            if (a == max(age_bins)) {
              find2 <- find[which(data[find, "allAs"] %in% c(Inf, a))]
            }
            if (!a %in% c(min(age_bins), max(age_bins))) {
              find2 <- which(data[find, "allAs"] == a)
            }
            comps_row <- c(comps_row, length(find2))
          } # End Age loop
          # Add to results matrix
          input_n <- c(input_n, sum(comps_row))
          sex <- c(sex, dplyr::case_when(s == "M" ~ 2, s == "F" ~ 1, .default = 0))
          year <- c(year, y)
          lbin_low <- c(lbin_low, l)
          comps_df <- rbind(comps_df, comps_row)
        } # length(Which)
      } # End Length loop
    } # End Year loop
  } # End Sex loop

  row_info <- data.frame(
    "year" = year,
    "month" = month,
    "fleet" = fleet,
    "sex" = sex,
    "partition" = partition,
    "ageerr" = ageerr,
    "Lbin_lo" = lbin_low,
    "Lbin_hi" = lbin_low,
    "input_n" = input_n
  )

  rownames(comps_df) <- NULL
  if (any(c("M", "F") %in% sex_loop)) {
    caal <- cbind(row_info, comps_df, comps_df)
    colnames(caal)[-c(1:9)] <- c(paste("f", age_bins, sep = ""), paste("m", age_bins, sep = ""))
    # 0 out the needed location by sex
    female_loc <- 10:(length(age_bins) + 9)
    male_loc <- (1 + max(female_loc)):ncol(caal)
    caal[which(caal[, "sex"] == 1), male_loc] <- caal[which(caal[, "sex"] == 1), male_loc] * 0
    caal[which(caal[, "sex"] == 2), female_loc] <- caal[which(caal[, "sex"] == 2), female_loc] * 0
    caal[which(caal[, "sex"] == 0), male_loc] <- caal[which(caal[, "sex"] == 0), male_loc] * 0
  } else {
    caal <- cbind(row_info, comps_df)
    colnames(caal)[-c(1:9)] <- paste("u", age_bins, sep = "")
  }

  if (any(caal[, "input_n"] == 0)) {
    # Remove any rows with no samples
    remove <- which(caal[, "input_n"] == 0)
    caal <- caal[-remove, ]
  }

  if (!is.null(dir)) {
    project <- dplyr::if_else(
      "project" %in% colnames(data),
      true = gsub(" ", "_", tolower(unique(data[, "project"]))),
      false = ""
    )
    bin_range <- paste0("a", min(age_bins), "-a", max(age_bins), "_l", min(len_bins), "-l", max(len_bins))
    species <- gsub(" ", "_", tolower(unique(data[, "common_name"])))[1]
    write.csv(
      caal,
      file = file.path(plotdir, paste0("survey_caal_bins_", bin_range, "_", species, "_", project, ".csv")),
      row.names = FALSE
    )
  }

  return(caal)
}
