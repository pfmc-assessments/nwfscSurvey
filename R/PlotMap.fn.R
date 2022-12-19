#' Plot two figures showing catch-per-unit-effort data
#'
#' Plot catch-per-unit-effort (CPUE) data across all years and by year
#' and save them to the disk. Figures are created using {ggplot2}.
#'
#'
#' @template dir
#' @param dat An object created by [PullCatch.fn()].
#' @param main A string that will be prepended to the name of the saved png
#'   (i.e., "NWFSC" results in a file called "NWFSC_CPUE_Map.png").
#' @param dopng Deprecated with {nwfscSurvey} 2.1 because providing a non-NULL
#'   value to `dir` can serve the same purpose as `dopng = TRUE` without the
#'   potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
#'   no longer have to specify `dopng` to save the plot as a png.
#' @param plot A vector of integers to specify which plots you would like. The
#'   default is to print both figures.
#'   1. coastwide data across all years
#'   2. coastwide data by year
#'
#' @author Chantel R. Wetzel
#' @export
#' @seealso
#' * `PullCatch.fn()`
#' * `plot_westcoast()`
#' @return Figures are saved to the disk according to which plots are asked
#' for in `plot`. Each of the specified files are saved to a directory called
#' `map_plots` inside of `dir`, the specified directory. No objects are
#' returned to the user. But, the figures are printed to new windows if they
#' are not saved to the disk.
#'
#' @examples
#' \dontrun{
#' PlotMap.fn(dat = catch_nwfsc_combo, plot = 1)
#' PlotMap.fn(dat = catch_nwfsc_combo, plot = 2)
#' }
#' @import ggplot2

PlotMap.fn <- function(dir = NULL,
                       dat,
                       main = NULL,
                       dopng = lifecycle::deprecated(),
                       plot = 1:2) {


  if (lifecycle::is_present(dopng)) {
    lifecycle::deprecate_warn(
      when = "2.1",
      what = "nwfscSurvey::PlotMap.fn(dopng =)"
    )
  }

  plotdir <- file.path(dir, "plots")
  check_dir(dir = plotdir)

  plot_names <- file.path(
    plotdir,
    paste0(
      main,
      ifelse(test = is.null(main), yes = "", no = "_"),
      c("cpue_map.png", "cpue_by_year_map.png")
    )
  )

  ind <- dat$cpue_kg_km2 > 0
  pos.cat <- dat[ind, ]
  neg <- dat[dat[, "cpue_kg_km2"] == 0, ]
  mid <- as.numeric(stats::quantile(pos.cat[, "cpue_kg_km2"], 0.50))
  max.size <- 12
  lon_range <- c(min(dat$Longitude_dd), max(dat$Longitude_dd))
  lat_range <- c(min(dat$Latitude_dd), max(dat$Latitude_dd))

  color <- c("#fffa00", "#ffcc00", "#ff7700", "#B60000")

  igroup <- 1
  if (igroup %in% plot) {

    g <- ggplot(dat) +
      geom_point(
        data = neg,
        aes(
          x = Longitude_dd, y = Latitude_dd,
          color = cpue_kg_km2, size = cpue_kg_km2
        ),
        pch = 1,
        col = "lightgrey",
        alpha = 0.15
      ) +
      geom_point(
        data = pos.cat,
        aes(
          x = Longitude_dd, y = Latitude_dd,
          color = cpue_kg_km2, size = cpue_kg_km2
        ),
        pch = 16,
        alpha = 0.7
      ) +
      scale_size_area(max_size = max.size, name = "CPUE kg/km2") +
      scale_color_gradient2(
        midpoint = mid,
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
      theme(legend.position = "right")
    print(g)

    if (!is.null(dir)) {
       ggsave(filename = plot_names[1], width = 7, height = 10, units = 'in')     
    }
  }

  igroup <- 2
  if (igroup %in% plot) {
    # By year
    plot_format <- theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

    h <- ggplot(dat) +
      geom_point(
        data = neg,
        aes(
          x = Longitude_dd, y = Latitude_dd,
          color = cpue_kg_km2, size = cpue_kg_km2
        ),
        pch = 1, 
        col = "lightgrey",
        alpha = 0.15
      ) +
      geom_point(
        data = pos.cat,
        aes(
          x = Longitude_dd, y = Latitude_dd,
          color = cpue_kg_km2, size = cpue_kg_km2
        ),
        pch = 16,
        alpha = 0.7
      ) +
      scale_size_area(max_size = 12, name = "CPUE kg/km2") +
      scale_color_gradient2(
        midpoint = mid,
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
      theme(legend.position = "right") +
      facet_wrap(~Year, ncol = 6)
    print(h)

    if (!is.null(dir)) {
       ggsave(filename = plot_names[2], width = 7, height = 10, units = 'in')     
    }
  }
}
