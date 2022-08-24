#' @param survey A character entry from one of the following options that
#' specifies which survey to pull the data for:
#'   * Triennial,
#'   * AFSC.Slope,
#'   * NWFSC.Combo,
#'   * NWFSC.Slope,
#'   * NWFSC.Shelf,
#'   * NWFSC.Hypoxia,
#'   * NWFSC.Santa.Barb.Basin,
#'   * NWFSC.Shelf.Rockfish (NWFSC.Hook.Line but both are not working),
#'   * NWFSC.Video.
#' Currently, you must pull data one survey at a time, though we are working on
#' allowing for a vector of survey names and
#' `NWFSC.Shelf.Rockfish` and `NWFSC.Hook.Line` are not supported.
#' The default of `NULL` is a placeholder that must be replaced with an entry.
