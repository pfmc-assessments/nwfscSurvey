#' Quantify number of observations by group
#'
#' @description [quantify()] lets you quickly quantify the number of
#' observations or distinct levels of variables within a group.
#' The function extends the functionality of [dplyr::count()] by
#' * allowing multiple variables via [dplyr::across()] and
#' * allowing users to pass a list of functions rather than assuming that
#'   all users are interested in [dplyr::n()].
#'
#' @details
#' The code for `quantify()` is roughly equivalent to a wrapper around
#' [dplyr::group_by()] and [dplyr::summarise()], i.e.,
#' ```
#'   data %>%
#'     group_by(...) %>%
#'     summarise("{{.col}}{{sep}}{{.fun}}" := ~)
#' ```
#' The names of the new columns are dynamically created by pasting the
#' string version of the column name with the name of the function.
#' This should help your future self know what variable was summarized and
#' what function was used for the summary. This combination of variable
#' and function is necessary because you cannot have duplicate column names.
#' The column name and function name will be concatenated with `sep`.
#' @template data
#' @template ldots_group
#' @param vars <[dplyr::dplyr_data_masking]>
#'   A vector of unquoted variable names.
#' @param fns A named list of functions. Only functions that accept vectors
#'   as their first argument will work, e.g.,
#'   [length()], [dplyr::n_distinct()].
#'   The default uses both of these functions, compared to [dplyr::count()]
#'   that is hard-coded to use [length()].
#'   The names of each function in the list are used to name the resulting
#'   column in the returned data frame.
#' @template sep
#'
#' @author Kelli F. Johnson
#' @export
#' @family dplyr verbs
#' @seealso
#' * [dplyr::count()] is a similar wrapper function but you cannot
#'   pass user-supplied functions
#' * [dplyr::across()] allows users to pass multiple variables to `vars`
#' @return A long data frame with
#' * a column for each grouping structure passed to `...` and
#' * columns based on all combinations of input variables and functions
#'   named using [glue::glue()], i.e., `"{{var}}{{sep}}{{fn}}"`.
#' Missing observations for each grouping structure are included in
#' the final output with a count of `0L` using `count(.drop = FALSE)`.
#' If `add_quantity()` was called, columns are added to `data`.
#'
#' @examples
#' # Using iris from {datasets} we rename the default functions to make
#' # the resulting column names more intuitive to non-fisheries users
#' # "N" is number of observations and "Unique" is number of unique values
#' # per Species
#' data("iris", package = "datasets")
#' quantify(
#'   iris,
#'   Species,
#'   vars = c(Petal.Length, Sepal.Length),
#'   fns = list(N = length, Unique = dplyr::n_distinct),
#'   sep = "_"
#' )
#'
#' # Repeat simple example above but add columns to iris
#' simple <- add_quantify(
#'   iris,
#'   Species,
#'   vars = c(Petal.Length, Sepal.Length),
#'   fns = list(N = length, Unique = dplyr::n_distinct),
#'   sep = "_"
#' )
#' # 4 new columns are added
#' dim(simple) - dim(iris)
#'
#' # Example with bio_nwfsc_combo by NA, Sex, Year
#' quantify(
#'   bio_nwfsc_combo,
#'   isNA = is.na(Length_cm),
#'   Sex_SS3 = codify_sex_SS3(Sex),
#'   Year = Year,
#'   vars = Trawl_id
#' )
#'
quantify <- function(data,
                     ...,
                     vars,
                     fns = list(N = length, NTow = n_distinct),
                     sep = "_") {

  out <- data %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(
      across(
        .cols = !!!dplyr::enquos(vars),
        .fns = fns,
        .names = quantify_names(sep = sep)
      )
    )

  # Ensure grouping is transient
  if (is.data.frame(data)) {
    out <- dplyr::dplyr_reconstruct(out, data)
  }

  return(out)
}

#' @export
#' @rdname quantify
add_quantify <- function(data,
                         ...,
                         vars,
                         fns = list(N = length, NTow = n_distinct),
                         sep = "_") {
  # TODO: Think about placing the new columns after the last grouping
  #       using lastdot, learned about .fn and rlang::sym from r-bloggers
  # https://www.r-bloggers.com/2021/02/using-functions-as-an-input-to-functions-with-dbplyr/
  lastdot <- tail(dplyr::ensyms(...), 1)[[1]]

  out <- data %>%
    dplyr::group_by(...) %>%
    dplyr::mutate(
      across(
        .cols = !!!dplyr::enquos(vars),
        .fns = fns,
        .names = quantify_names(sep = sep)
      )
    ) %>%
    dplyr::ungroup()

  # Ensure grouping is transient
  if (is.data.frame(data)) {
    out <- dplyr::dplyr_reconstruct(out, data)
  }

  return(out)
}

quantify_names <- function(sep = "_") {
  glue::glue("{{.col}}{sep}{{.fn}}")
}
