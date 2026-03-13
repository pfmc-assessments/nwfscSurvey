#' Plot two figures showing catch-per-unit-effort data
#'
#' Plot catch-per-unit-effort (CPUE) data across all years and by year
#' and save them to the disk. Figures are created using {ggplot2}.
#'
#'
#' @inheritParams pull_catch
#' @param data A dataframe of catch data created by [pull_catch()].
#' @param main A string that will be prepended to the name of the saved png
#'   (i.e., "NWFSC" results in a file called "NWFSC_CPUE_Map.png").
#' @param plot A vector of integers to specify which plots you would like. The
#'   default is to print both figures.
#'   1. coastwide data across all years
#'   2. coastwide data by year
#'
#' @author Chantel R. Wetzel
#' @export
#' @family plot_
#' @seealso
#' * `pull_catch()`
#' * `plot_westcoast()`
#' @return Figures are saved to the disk according to which plots are asked
#' for in `plot`. Each of the specified files are saved to a directory called
#' `map_plots` inside of `dir`, the specified directory. No objects are
#' returned to the user. But, the figures are printed to new windows if they
#' are not saved to the disk.
#'
#' @examples
#' \dontrun{
#' plot_cpue_map(data = catch_nwfsc_combo, plot = 1)
#' plot_cpue_map(data = catch_nwfsc_combo, plot = 2)
#' }
#'
plot_cpue_map <- function(
  data,
  dir = NULL,
  main = NULL,
  plot = 1:2
) {
  plotdir <- file.path(dir)
  check_dir(dir = plotdir)

  plot_names <- file.path(
    plotdir,
    paste0(
      main,
      ifelse(test = is.null(main), yes = "", no = "_"),
      c("cpue_map.png", "cpue_by_year_map.png")
    )
  )

  modified_data <- data |>
    dplyr::rename_with(tolower)

  pos_catch <- modified_data |>
    dplyr::filter(cpue_kg_km2 > 0) |>
    dplyr::mutate(
      mid = stats::quantile(cpue_kg_km2, 0.50)
    )

  neg <- modified_data[modified_data[, "cpue_kg_km2"] == 0, ]
  max_circ_size <- 12
  lon_range <- c(
    min(modified_data$longitude_dd),
    max(modified_data$longitude_dd)
  )
  lat_range <- c(
    min(modified_data$latitude_dd),
    max(modified_data$latitude_dd)
  )

  color <- c("#fffa00", "#ffcc00", "#ff7700", "#B60000")

  igroup <- 1
  if (igroup %in% plot) {
    g <- ggplot2::ggplot(modified_data) +
      ggplot2::geom_point(
        data = neg,
        ggplot2::aes(
          x = longitude_dd,
          y = latitude_dd,
          color = cpue_kg_km2,
          size = cpue_kg_km2
        ),
        pch = 1,
        col = "lightgrey",
        alpha = 0.15
      ) +
      ggplot2::geom_point(
        data = pos_catch,
        ggplot2::aes(
          x = longitude_dd,
          y = latitude_dd,
          color = cpue_kg_km2,
          size = cpue_kg_km2
        ),
        pch = 16,
        alpha = 0.7
      ) +
      ggplot2::scale_size_area(max_size = max_circ_size, name = "CPUE kg/km2") +
      ggplot2::scale_color_gradient2(
        midpoint = unique(pos_catch$mid),
        low = color[2],
        mid = color[3],
        high = color[4],
        space = "Lab",
        name = "CPUE kg/km2"
      ) +
      draw_theme() +
      draw_projection() +
      draw_land() +
      draw_USEEZ(lon_range, lat_range) +
      label_land() +
      label_axes() +
      ggplot2::theme(legend.position = "right")
    print(g)

    if (!is.null(dir)) {
      ggplot2::ggsave(
        filename = plot_names[1],
        width = 7,
        height = 10,
        units = "in"
      )
    }
  }

  igroup <- 2
  if (igroup %in% plot) {
    # By year
    plot_format <- ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

    h <- ggplot2::ggplot(modified_data) +
      ggplot2::geom_point(
        data = neg,
        ggplot2::aes(
          x = longitude_dd,
          y = latitude_dd,
          color = cpue_kg_km2,
          size = cpue_kg_km2
        ),
        pch = 1,
        col = "lightgrey",
        alpha = 0.15
      ) +
      ggplot2::geom_point(
        data = pos_catch,
        ggplot2::aes(
          x = longitude_dd,
          y = latitude_dd,
          color = cpue_kg_km2,
          size = cpue_kg_km2
        ),
        pch = 16,
        alpha = 0.7
      ) +
      ggplot2::scale_size_area(max_size = 12, name = "CPUE kg/km2") +
      ggplot2::scale_color_gradient2(
        midpoint = unique(pos_catch$mid),
        low = color[2],
        mid = color[3],
        high = color[4],
        space = "Lab",
        name = "CPUE kg/km2"
      ) +
      plot_format +
      draw_theme() +
      draw_projection() +
      draw_land() +
      draw_USEEZ(lon_range, lat_range) +
      label_axes() +
      ggplot2::theme(legend.position = "right") +
      ggplot2::facet_wrap(~year, ncol = 6)
    print(h)

    if (!is.null(dir)) {
      ggplot2::ggsave(
        filename = plot_names[2],
        width = 7,
        height = 10,
        units = "in"
      )
    }
  }
}
