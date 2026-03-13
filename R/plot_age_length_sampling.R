#' Plot the representativeness of age sampling based on lengths
#'
#' @param data data frame
#' @param xlim Deprecated with {nwfscSurvey} version 2.8.0.1 because the limits
#'   are now automatically determined. x limits for plot, defaults to c(0,120)
#' @param ylim Deprecated with {nwfscSurvey} version 2.8.0.1 because the limits
#'   are now automatically determined. y limits for plot, defaults to (0, 0.049)
#' @param dir Defaults to NULL (plot made, but not saved to file). Can alternatively be a string filename
#'   by year and sex
#' @param width,height Numeric values for the figure width and height in
#'   inches. The defaults are 7 by 7 inches.
#'
#' @author Chantel Wetzel
#' @importFrom graphics grid hist
#' @export

plot_age_length_sampling <- function(
  data,
  dir = NULL,
  height = 7,
  width = 7,
  xlim = lifecycle::deprecated(),
  ylim = lifecycle::deprecated()
) {
  if (lifecycle::is_present(xlim)) {
    lifecycle::deprecate_warn(
      when = "2.8.0.1",
      what = "nwfscSurvey::plot_age_length_sampling(xlim =)"
    )
  }
  if (lifecycle::is_present(ylim)) {
    lifecycle::deprecate_warn(
      when = "2.8.0.1",
      what = "nwfscSurvey::plot_age_length_sampling(ylim =)"
    )
  }
  if (!is.null(dir)) {
    filename <- file.path(dir, "age_length_comparison.png")
  }
}
