#' @param .keep  Control which columns from `data` are retained in the output.
#'   Grouping columns and columns created by `...` are always kept.
#'   The default is `"all"`, which is the first option listed. Details about
#'   the following options were copied from the documentation of
#'   `dplyr::mutatedata.frame()`:
#'
#'   * `"all"` retains all columns from `data`. This is the default.
#'   * `"used"` retains only the columns used in `...` to create new
#'     columns. This is useful for checking your work, as it displays inputs
#'     and outputs side-by-side.
#'   * `"unused"` retains only the columns _not_ used in `...` to create new
#'     columns. This is useful if you generate new columns, but no longer need
#'     the columns used to generate them.
#'   * `"none"` doesn't retain any extra columns from `data`. Only the grouping
#'     variables and columns created by `...` are kept.
