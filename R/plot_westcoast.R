#' Plot the United States west coast using {ggplot2}
#'
#' Plot the United States west coast such that it can used as a standalone
#' figure or as a base to plot data points on.
#'
#' @author Kelli F. Johnson
#' @export
#' @return A {ggplot2} object.
#'
#' @seealso
#' * `PlotMap.fn()` uses this function as the base map.
#'
#' @examples
#' map_object <- plot_westcoast()
#' \dontrun{
#' print(map_object)
#' ggplot2::ggsave(
#'   filename = "myfilename.png",
#'   plot =  map_object
#' )
#' }
plot_westcoast <- function() {
  ggplot2::ggplot() +
    draw_theme() +
    draw_projection() +
    draw_land() +
    draw_USEEZ() +
    label_land() +
    label_axes()
}
