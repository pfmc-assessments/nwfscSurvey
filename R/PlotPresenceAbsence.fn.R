#' Plot showing presence and absence per haul by depth, latitude, or sex bin
#'
#' This function was deprecated in {nwfsc} version 2.1. Please use
#' [plot_proportion()] instead.
#'
#' @param ... Any arguments associated with now-deprecated function
#' @export
PlotPresenceAbsence.fn <- function(...) {
  lifecycle::deprecate_stop(
    when = "2.1",
    what = "PlotPresenceAbsence.fn()",
    with = "plot_proportion()"
  )
}
