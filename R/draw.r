#' @export
draw_land <- function() {
  # Get world data so islands are plotted
  info_world <- ggplot2::map_data(
    map = "world",
    region = c("USA", "Canada", "Mexico")
  )

  # Get state data to plot needed state lines
  # Turn off coastal lines of Oregon to eliminate double line and
  # small inconsistencies between info_world and info_state
  info_state <- ggplot2::map_data(
    map = "state",
    region = c("idaho", "oregon", "nevada")
  )
  info_state[, "subregion"] <- "inland"
  info_state[info_state[, "region"] == "oregon", ][
    71:144, "subregion"
  ] <- "coast"

  list(
    ggnewscale::new_scale_color(),
    ggplot2::geom_polygon(
      data = info_world,
      ggplot2::aes(x = long, y = lat, group = group),
      color = "black",
      fill = "lightgray",
      size = 0.25,
      inherit.aes = FALSE
    ),
    ggplot2::geom_path(
      data = info_state,
      ggplot2::aes(x = long, y = lat, group = group, color = subregion),
      size = 0.25,
      show.legend = FALSE,
      inherit.aes = FALSE
    ),
    ggplot2::scale_colour_manual(
      # Black for state boundaries and transparent for outer coast
      # to eliminate the double line
      values = c(inland = "black", coast = "#FF000000")
    ),
    ggnewscale::new_scale_color()
  )
}

#' @export
draw_projection <- function() {
  # Set the projection outside of the {ggplot2} object to avoid the
  # warning about resetting the coordinates.
  # See https://github.com/tidyverse/ggplot2/issues/2799
  projection <- ggplot2::coord_map(projection = "mercator")
  projection[["default"]] <- TRUE
  list(
    projection
  )
}

#' A {ggplot2} theme for drawing maps
#'
#' @param size A real number specifying the line width, or size, for the panel
#'   border around the figure. The default is `0.25`.
#' @export
draw_theme <- function(size = 0.25) {
  list(
    theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        size = size
      )
    )
  )
}

#' Bounds the figure with the limits of the U.S. EEZ
#'
#' @param lon,lat Vectors of two real numbers specifying the longitudinal and
#'   latitudinal limits for the x and y axes.
#' @export
draw_USEEZ <- function(lon = c(-129.15, -116.50),
                       lat = c(31.90, 49.50)) {
  list(
    ggplot2::coord_sf(
      xlim = lon,
      ylim = lat
    )
  )
}
