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
#' caal_data <- get_raw_comps(
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

  Results = NULL
  sex_loop <- unique(data[, "sex"])
  year_loop <- unique(data[, "year"])

  #Loop across F then M
  for(s in sex_loop){
    # Loop across years
    year_loop <- sort(unique(data[, "year"][data[, "sex"] == s]))
    for(y in year_loop){
      ########## CONDITIONAL
      # Loop across Length-bins
      for(l in len_bins){
        # Identify relevant rows
        if(l == min(len_bins)) {
          find = which(data[, "sex"] == s & data[, "year"] == y & data[,'allLs'] %in% c(-999, l))
        }
        if(l == max(len_bins)){
          find = which(data[, "sex"] == s & data[, "year"] == y & data[,'allLs'] %in% c(Inf, l))
        }
        if(!l %in% c(min(len_bins), max(len_bins))){
          find = which(data[, "sex"] == s & data[, "year"] == y & data[,'allLs'] == l)
        }
        # Skip this year unless there are rows
        if(length(find) > 0){
          # Format reference stuff
          Row = c(
            'year' = y,
            'month' = month,
            'fleet'= fleet,
            'sex' = ifelse(s == "F", 1, ifelse(s == "M", 2, 0)),
            'partition' = partition,
            'age_error' = ageerr,
            'Lbin_lo' = l,
            'Lbin_hi' = l,
            'input_n'= NA)
          # Loop across age bins
          for(a in age_bins){
            # Subset to relevant rows
            if(a == min(age_bins)) {
              find2 = find[which(data[find, "allAs"] %in% c(-999, a))]
            }
            if(a == max(age_bins)){
              find2 = find[which(data[find, "allAs"] %in% c(Inf, a))]
            }
            if(!a %in% c(min(age_bins), max(age_bins))){
              find2 = which(data[find,'allAs'] == a)
            }
            Row = c(Row, length(find2))
          } # End Age loop
          # Add to results matrix
          Row['input_n'] <- sum(as.numeric(Row[10:length(Row)]))
          Results = rbind(Results, Row)
        } # length(Which)
      } # End Length loop
    } # End Year loop
  } # End Sex loop

  # Add headers
  Results <-  data.frame(Results)
  caal <- cbind(Results, Results[, 10:ncol(Results)])
  colnames(caal)[-c(1:9)] = c(paste("f", age_bins, sep=""), paste("m", age_bins, sep=""))

  if (!is.null(dir)) {
    project <- dplyr::if_else(
      "project" %in% colnames(data),
      true = gsub(" ", "_", tolower(unique(data[, "project"]))),
      false = ""
    )
    bin_range <- paste0("a", min(age_bins), "-a", max(age_bins), "_l", min(len_bins), "-l", max(len_bins))

    write.csv(
      caal,
      file = file.path(plotdir, paste0("survey_caal_bins_", bin_range,"_", species, "_", project, ".csv")),
      row.names = FALSE
    )
  }

  return(caal)
}
