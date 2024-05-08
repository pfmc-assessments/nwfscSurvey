#' Plot showing presence and absence per haul by depth, latitude, or sex bin
#'
#' This function was deprecated in {nwfsc} version 2.1. Please use
#' [plot_proportion()] instead.
#'
#' @param data data.frame containing data per haul created by [PullCatch.fn()]
#'   or biological data created by [PullBio.fn()] where the `dim = "sex"` must
#'   be true for the latter.
#' @param dim Dimension of interest, either "depth", "lat", or "sex".
#' @template dir
#' @param plot_type Two options area available "proportion" or "total" where the
#'   default, "proportion", plot the proportion by depth/latitude with equal bar
#'   widths and the "total" option plot the numbers by depth/latitude were the
#'   bar widths in a production of sampling by depth/latitude.
#' @param dopng Deprecated with {nwfscSurvey} 2.1 because providing a non-NULL
#'   value to `dir` can serve the same purpose as `dopng = TRUE` without the
#'   potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
#'   no longer have to specify `dopng` to save the plot as a png.
#' @param depth_min Minimum depth (in meters).
#' @param depth_max Maximum depth (in meters). A NULL value will cause the
#'   function to automatically set depth_max to the multiple of depth_bin_width
#'   beyond the 99.9 percentile of the observations.
#' @param depth_bin_width Width of each depth bin (in meters).
#' @param lat_min Minimum latitute (in decimal degrees).
#' @param lat_max Maximum latitute (in decimal degrees).
#' @param lat_bin_width Width of each latitude bin (in decimal degrees).
#' @param add_range_to_main Add the range of latitude or depth by which the data
#' are filtered.
#' @param xlab Label for x-axis. A NULL value will cause the function
#' to choose either "Depth (m)" or "Latitude (°N)".
#'
#' @author Ian G. Taylor and Chantel Wetzel
#' @importFrom grDevices gray
#' @export
#' @examples
#' \dontrun{
#' # load WCGBTS data data
#' data.WCGBTS.ling <- nwfscSurvey::PullCatch.fn(
#'   Name = "lingcod",
#'   SurveyName = "NWFSC.Combo"
#' )
#' bio.WCGBTS.ling <- nwfscSurvey::PullBio.fn(
#'   Name = "lingcod",
#'   SurveyName = "NWFSC.Combo"
#' )
#' PlotPresenceAbsence.fn(data = data.WCGBTS.ling, dim = "lat")
#' PlotPresenceAbsence.fn(data = data.WCGBTS.ling, dim = "depth")
#' PlotPresenceAbsence.fn(data = bio.WCGBTS.ling, dim = "sex")
#'
#' # Plot with package data
#' PlotPresenceAbsence.fn(catch_nwfsc_combo)
#' }
PlotPresenceAbsence.fn <- function(
  data,
  dim = c("depth", "lat", "sex"),
  dir = NULL,
  dopng = lifecycle::deprecated(),
  plot_type = c("proportion", "total"),
  depth_min = 50,
  depth_max = NULL,
  depth_bin_width = 25,
  lat_min = 32,
  lat_max = 49,
  lat_bin_width = 1.0,
  add_range_to_main = TRUE,
  xlab = NULL) {

  lifecycle::deprecate_soft(
    when = "2.1",
    what = "PlotPresenceAbsence.fn()",
    with = "plot_proportion()"
  )

  dim <- match.arg(dim)
  plot_type <- match.arg(plot_type)

  plot_proportion(
    data = data,
    dim = dim,
    dir = dir,
    plot_type = plot_type,
    depth_min = depth_min,
    depth_max = depth_max,
    depth_bin_width = depth_bin_width,
    lat_min = lat_min,
    lat_max = lat_max,
    lat_bin_width = lat_bin_width,
    add_range_to_main = add_range_to_main,
    xlab = xlab
  )

}
