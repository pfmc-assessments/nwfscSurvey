# Documentation ----
#' Bin and count observations in each bin by group
#'
#' @description
#' `bin()` returns counts of one or more variables for group/bin combinations.
#' This is similar to [dplyr::count()] but adds the additional step of creating
#' one new column per variable that defines which bin it belongs to before
#' counting them. For each variable of interest two new columns are created.
#' Columns will be dynamically named according to the following calls to
#' [glue::glue()]: `"{.col}{.sep}bin"` and `"{.col}{.sep}n"`.
#' These columns are returned as well as
#' any groupings already present in `data` or added via `...`.
#'
#' `add_bin()` is equivalent to `bin()` but uses [dplyr::mutate()] instead of
#' [dplyr::summarise()]. So, it adds two columns per variable of interest
#' to the original data frame, rather than
#' summarizing the returned data frame to the group level.
#'
#' @details
#' The code for `bin()` is roughly equivalent to
#' ```
#'   data %>%
#'     mutate("{.col}{.sep}bin" := cut()) %>%
#'     group_by(...) %>%
#'     summarise("{.col}{.sep}n" := n())
#' ```
#' but uses functionality in [dplyr::across()] facilitate calling `bin()`
#' just one time even if you are interested in adding bin information
#' for multiple variables. Essentially [dplyr::mutate()] is called once for
#' each vector present in `bin_list`.
#'
#' `bin()` uses the functionality of \pkg{glue} and a hidden function in
#' \pkg{dplyr} called `dplyr:::across_glue_mask()` to name the new columns.
#' The column name that is currently of interest is pasted together with
#' the argument `.sep` and `"bin"` or `"n"` to create the new names.
#' Pasting the suffixes to the original column name is both
#' helpful and necessary because data frames cannot have multiple columns with
#' the same name. That is, you can only run [dplyr::count()] once on a data
#' frame because the result is always named `"n"`. So, `bin()` saves you the
#' trouble of having to rename the column later.
#'
#' `bin()` uses [cut()] to determine the interval, or bin, for each row.
#' Specifically, `cut(right = FALSE, include.lowest = TRUE)`
#' is hard coded in `bins()`, to ensure that
#' bins are closed on the left and open on the right,
#' e.g., `"[0,20)`.
#' Except for the last bin, which is closed on the right.
#' For open intervals, indicated using round brackets or parentheses,
#' the endpoint is not included in the interval.
#' For closed intervals, indicated using square brackets,
#' the endpoint is included in the interval.
#' Thus,
#' `[0,` describes the left side of an interval greater than or equal to 0 and
#' `,20)` describes the right side of an interval less than 20.
#' This notation of round and square brackets is used for the strings in each
#' new `*bin` column such that the lower and upper limits of the bins are clear.
#'
#' Missing observations for each grouping structure are included in
#' the final output of `bin()` with a count of `0L` using
#' `dplyr::group_by(.drop = FALSE)`. Thus, if you add grouping structures to
#' your data outside of `bin()` this behavior might be lost.
#' @template data
#' @template ldots_group
#' @param bin_list A list of vectors with one vector per variable of
#'   interest. The list should be named such that the function knows
#'   which variable each binning vector pertains to.
#'   For each vector, there must be two or more numerical values which
#'   will be passed to
#'   `cut(breaks = bins, right = FALSE, include.lowest = TRUE)`.
#'   The first value will be the left limit of the first bin,
#'   which will be a closed interval to include values equal to the input.
#'   The last value will be the right limit of the last bin,
#'   which will also be a closed interval to include values equal to the input.
#'   Many people like to add `-999` and `Inf` to the beginning and end of
#'   the input vector, respectively, to ensure that
#'   all positive values will be assigned to a bin rather than `NA`.
#'   A message will be printed to the screen if each vector does not lead to all
#'   of your data being placed in a bin, e.g., if the maximum value in your data
#'   exceeds the right limit of the last bin.
#'   See the details section or `?cut` for more information.
#' @template .sep
#' @template .keep
#'
#' @author Kelli F. Johnson
#' @family dplyr verbs
#' @return A long data frame with
#' * a column for each grouping structure passed to `...`,
#' * a column of strings called `"*{.sep}bin"`
#'   with the lower and upper limits of the bin, and
#' * a column of integers called `*{.sep}n"`
#'   with the number of observations.
#'
#' @examples
#' # An example with the iris dataset that groups Sepal.Length
#' # by Species and 2 bins, resulting in 3 x 2 groups or 6 rows
#' bin(data = iris, Species, bin_list = list(Sepal.Length = c(0, 5, Inf)))
#' # Same example, but use add_bin() to add _bin and _n to iris
#' # Use head() to just print the first few columns
#' add_bin(
#'   data = iris,
#'   Species,
#'   bin_list = list(Sepal.Length = c(0, 5, Inf))
#' ) %>% head
#'
#' # Use add_bin() to add 2 columns to data 1-column data frame
#' # (result has same number of rows as data)
#' simple <- add_bin(
#'   data.frame(x = rep(1:5, 2)),
#'   bin_list = list(x = c(0, 5, Inf))
#' )
#' NROW(simple) == 10
#' colnames(simple)
#'
#' # Remaining examples use the {nwfscSurvey} biological data
#' # Summarize lengths in bins by Year and Sex (result has < rows than input)
#' # Final bin [200,Inf] is a plus group with all measurements 200 cm and above
#' summary <- bin(
#'   data = bio_nwfsc_combo,
#'   Year, Sex,
#'   bin_list = list(Length_cm = c(seq(0, 200, by = 20), Inf))
#' )
#' # Equal number of rows per bin because missing observations per group are
#' # filled in and sum of counts should equal the number of rows of data
#' dplyr::count(summary, Length_cm_bin)
#'
#' # add_bin() works like dplyr::mutate() instead of dplyr::summarise()
#' # (result of add_bin() has same number of rows as input)
#' original <- add_bin(
#'   data = bio_nwfsc_combo %>% group_by(Year, Sex),
#'   bin_list = list(Length_cm = seq(0, 300, by = 20))
#' )
#' # The following have different counts per bin because bins in original
#' # are closed on the left and open on the right and when we use bespoke
#' # dplyr code here, the bins are open on left.
#' bio_nwfsc_combo %>%
#'  dplyr::mutate(bin = cut(Length_cm, breaks = seq(0, 300, by = 20))) %>%
#'  group_by(Year, Sex = codify_sex(Sex), bin) %>%
#'  pull(bin) %>% table(., useNA = "always")
#' table(original[["Length_cm_bin"]])
#' sum(bio_nwfsc_combo$Length_cm <= 20, na.rm = TRUE)
#' sum(bio_nwfsc_combo$Length_cm < 20, na.rm = TRUE)
#'
#' # clean up
#' # rm(summary, original, simple)

# Exported functions ----
#' @rdname bin
#' @export
bin <- function(data,
                ...,
                bin_list,
                .sep = "_") {
  # Setup
  suffix <- paste0(.sep, "bin")
  mydots <- rlang::enquos(...)

  data_binned <- bin_bin(
    data = data,
    bin_list = bin_list,
    suffix = suffix,
    .keep = "all"
  )

  # Add 1 integer column (*_n) of counts per bin per list element in bin_list
  data_counts <- purrr::map(
    .x = grep(suffix, x = colnames(data_binned), value = TRUE),
    .f = ~ data_binned %>%
      dplyr::group_by(!!!mydots, .add = TRUE, .drop = FALSE) %>%
      dplyr::group_by(!!rlang::sym(.x), .add = TRUE, .drop = FALSE) %>%
      # The following line is the only difference between bin and add_bin
      # TODO: decrease redundant code
      # summarize("{gsub('bin$', 'n', {.x})}" := n())
      count(.drop = FALSE, name = "{.col}_n")
  ) %>%
    purrr::reduce(dplyr::full_join, by = colnames(data_binned))


  data_out <- check_grouping(data, data_counts)
  return(data_out)
}

#' @rdname bin
#' @export
add_bin <- function(data,
                    ...,
                    bin_list,
                    .sep = "_",
                    .keep = c("all", "used", "unused", "none")) {
  # Setup
  suffix <- paste0(.sep, "bin")
  mydots <- rlang::enquos(...)

  data_binned <- bin_bin(
    data = data,
    bin_list = bin_list,
    suffix = suffix,
    .keep = .keep
  )

  # Add 1 integer column (*_n) of counts per bin per list element in bin_list
  data_counts <- purrr::map(
    .x = grep(suffix, x = colnames(data_binned), value = TRUE),
    .f = ~ data_binned %>%
      dplyr::group_by(!!!mydots, .add = TRUE, .drop = FALSE) %>%
      dplyr::group_by(!!rlang::sym(.x), .add = TRUE, .drop = FALSE) %>%
      # The following line is the only difference between bin and add_bin
      # TODO: decrease redundant code
      mutate("{gsub('bin$', 'n', {.x})}" := n())
  ) %>%
    purrr::reduce(dplyr::full_join, by = colnames(data_binned))

  data_out <- check_grouping(data, data_counts)
  return(data_out)
}

# Helper functions ----
# The remainder of this file is helper functions for bin() and add_bin()

bin_bin <- function(data,
                    bin_list,
                    suffix = "_bin",
                    .keep = c("all", "used", "unused", "none")) {
  # Setup
  stopifnot(is.list(bin_list))
  stopifnot(all(names(bin_list) %in% colnames(data)))
  .keep <- match.arg(.keep)

  # Add 1 string column (*_bin) of bin info per list element in bin_list
  dplyr::mutate(
    .data = data,
    dplyr::across(
      .cols = names(bin_list),
      .fns = ~ cut_assess(
        x = .x,
        breaks = bin_list[[cur_column()]]
      ),
      .names = "{paste0({col}, suffix)}"
    ),
    .keep = .keep
  )

}

cut_assess <- function(x, breaks) {
  minbreak <- min(breaks, na.rm = TRUE)
  if (minbreak > min(x, na.rm = TRUE)) {
    message(
      "The smallest bin starts at ",
      minbreak,
      " and does not include your minimum value, ",
      min(x, na.rm = TRUE),
      "."
    )
  }

  if (last(breaks) < max(x, na.rm = TRUE)) {
    message(
      "The largest bin does not include your max value, ",
      max(x, na.rm = TRUE),
      " consider adding Inf to breaks."
    )
  }

  # Tested findInterval() vs. cut() for speed, equal performance;
  # findInterval() returns the i of bin[i], where
  # cut() returns a factor specifying the bin, e.g., `"[b1, b2)"`
  cut(x, breaks = breaks, include.lowest = TRUE, right = FALSE)
}
