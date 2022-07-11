#' Count observations in each bin by group
#'
#' @description
#' `bin()` lets you quickly count the number of observations within defined
#' bins per group.
#' This is similar to [dplyr::count()] but on a newly-created,
#' dynamically-named column containing the binning categories.
#'
#' `add_bin()` is equivalent to `bin()` but uses [dplyr::mutate()] instead of
#' [dplyr::summarise()]. So, it adds two columns to the original data frame,
#' rather than summarizing the returned data frame to the group levels.
#'
#' @details
#' The code for `bin()` is roughly equivalent to
#' ```
#'   data %>%
#'     group_by(...) %>%
#'     mutate("{{var}}_bin" := cut()) %>%
#'     summarise("{{var}}_n" := n())`
#' ```
#' The names of the two new columns are dynamically created by pasting the
#' string version of `var` with `"_bin"` and `"_n"`. This should help your
#' future self know what variable the bins and counts are summarizing and
#' it is also necessary because [dplyr::count()] will not work if there
#' is already a column in `data` called `"n"`. Thus, the dynamic naming
#' allows you to summarize multiple biological variables by running
#' `add_bin()` more than once.
#'
#' `bin()` uses [cut()] to determine the interval for each row.
#' `cut(right = FALSE, include.lowest = TRUE)` is hard coded, ensuring bins
#' are closed on the left and open on the right, e.g., `"[0,20)`.
#' Except for the last bin, which is closed on the right.
#' For open intervals, indicated using round brackets or parentheses,
#' the endpoint is not included in the interval.
#' For closed intervals, indicated using square brackets,
#' the endpoint is included in the interval.
#' Thus,
#' `[0,` describes the left side of an interval greater than or equal to 0 and
#' `,20)` describes the right side of an interval less than 20.
#' This notation of round and square brackets is used to name the bins such
#' that the lower and upper limits of the bins are clear.
#'
#' @template data
#' @template ldots_group
#' @param var <[dplyr::dplyr_data_masking]>
#'   A variable, without quotes, that is passed to `cut(x = data[[var]])`,
#'   which divides the data into intervals based on `bins` and codes each row
#'   according to the interval in which it falls.
#'   Must be a valid column present in `data`.
#' @template bins
#' @template sep
#'
#' @export
#' @author Kelli F. Johnson
#' @family dplyr verbs
#' @return A long data frame with
#' * a column for each grouping structure passed to `...`,
#' * a column of strings called `"{{var}}_bin"`
#'   with the lower and upper limits of the bin, and
#' * a column of integers called `{{var}}_n"`
#'   with the number of observations.
#' Missing observations for each grouping structure are included in
#' the final output with a count of `0L` using `count(.drop = FALSE)`.
#' If `add_bin()` was called, then `{{var}}_bin"` and `{{var}}_n"`
#' are added to `data`.
#'
#' @examples
#' # A simple example with the iris dataset that groups Sepal.Length
#' # by Species and 2 bins
#' data(iris, package = "datasets")
#' bin(data = iris, Species, var = Sepal.Length, bins = c(0, 5, Inf))
#'
#' # Simple example of add_bin() that adds 2 columns to data
#' # (result has same number of rows as data)
#' simple <- add_bin(
#'   data.frame(x = rep(1:5, 2)),
#'   var = x,
#'   bins = c(0, 5, Inf)
#' )
#' NROW(simple) == 10
#' colnames(simple)
#'
#' # Remaining examples use the {nwfscSurvey} biological data
#' # Summarize lengths in bins by Year and Sex (result has < rows than input)
#' # Final bin [200,Inf] is a plus group with all measurements 200 cm and above
#' summary <- bin(
#'   data = bio_nwfsc_combo,
#'   Year, Sex = codify_sex(Sex),
#'   var = Length_cm,
#'   bins = c(seq(0, 200, by = 20), Inf)
#' )
#' # Equal number of rows per bin because missing observations per group are
#' # filled in and sum of counts should equal the number of rows of data
#' dplyr::count(summary, Length_cm_bin)
#' sum(summary[["Length_cm_n"]]) == NROW(bio_nwfsc_combo)
#'
#' # add_bin() works like dplyr::mutate() instead of dplyr::summarise()
#' # (result has same number of rows as input)
#' original <- add_bin(
#'   data = bio_nwfsc_combo %>% group_by(Year, Sex = codify_sex(Sex)),
#'   var = Length_cm,
#'   bins = seq(0, 300, by = 20)
#' )
#' # The following have different counts per bin because bins in original
#' # are closed on the right and open on the left and when we use bespoke
#' # dplyr code here, the bins are open on right.
#' bio_nwfsc_combo %>%
#'  dplyr::mutate(bin = cut(Length_cm, breaks = seq(0, 300, by = 20))) %>%
#'  group_by(Year, Sex = codify_sex(Sex), bin) %>%
#'  pull(bin) %>% table(., useNA = "always")
#' table(original[["Length_cm_bin"]])
#' sum(bio_nwfsc_combo$Length_cm < 20, na.rm = TRUE)
#' sum(bio_nwfsc_combo$Length_cm <= 20, na.rm = TRUE)
#'
#' # clean up
#' # rm(summary, original, simple)
#'
bin <- function(data,
                ...,
                var,
                bins,
                sep = "_") {

  out <- data %>%
    dplyr::mutate("{{var}}_bin" := cut_assess(x = {{var}}, breaks = bins)) %>%
    dplyr::count(
      ..., !!sym(paste0(ensym(var), sep, "bin")),
      name = paste0(ensym(var), sep, "n"),
      .drop = FALSE
    )

  # Ensure grouping is transient
  if (is.data.frame(data)) {
    out <- dplyr::dplyr_reconstruct(out, data)
  }

  return(out)
}

#' @rdname bin
#' @export
add_bin <- function(data,
                    ...,
                    var,
                    bins,
                    sep = "_") {
  out <- data %>%
    dplyr::mutate("{{var}}_bin" := cut_assess(x = {{var}}, breaks = bins)) %>%
    dplyr::add_count(
      ..., !!sym(paste0(ensym(var), sep, "bin")),
      name = paste0(ensym(var), sep, "n")
    ) %>%
    dplyr::ungroup()

  # Ensure grouping is transient
  if (is.data.frame(data)) {
    out <- dplyr::dplyr_reconstruct(out, data)
  }

  return(out)
}

cut_assess <- function(x, breaks) {
  if (!0 %in% breaks) {
    warning(
      "The smallest bin starts with ",
      min(breaks, na.rm = TRUE),
      " but you should likely start with 0L."
    )
  }

  if (last(breaks) < max(x, na.rm = TRUE)) {
    warning(
      "The largest bin does not include your max value, ",
      max(x),
      " consider adding Inf to breaks."
    )
  }

  # Tested findInterval() vs. cut() for speed, equal performance;
  # findInterval() returns the i of bin[i], where
  # cut() returns a factor specifying the bin, e.g., `"[b1, b2)"`
  cut(x, breaks = breaks, include.lowest = TRUE, right = FALSE)
}
