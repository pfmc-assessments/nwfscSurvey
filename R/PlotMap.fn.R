#' Plot two figures showing catch-per-unit-effort data
#'
#' Plot catch-per-unit-effort (CPUE) data across all years and by year
#' and save them to the disk. Figures are created using {ggplot2}.
#'
#' @param dir The directory where you would like the figures saved. The
#'   directory must exist; but, it is only needed if `dopng = TRUE`.
#' @param dat A data frame created by `PullCatch.fn()`.
#' @param main A string that will be pre-pended to the default file names for
#'   each figure. For example, if `main = "NWFSC"`, then one of the saved
#'   `.png` files will be called `"NWFSC_CPUE_Map.png"`.
#' @param dopng A logical value specifying if the figures should be saved as
#'   `.png` files. The default is `FALSE`, which leads to the figures being
#'   shown in an R window. If `dopng = TRUE`, then figures are saved to the
#'   disk.
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
                       dopng = FALSE,
                       plot = 1:2) {

  # Create specialized plots
  pngfun <- function(dir, file, w = 7, h = 7, pt = 12) {
    file <- file.path(dir, file)
    cat("writing PNG to", file, "\n")
    png(
      filename = file,
      width = w, height = h,
      units = "in", res = 300, pointsize = pt
    )
  }


  if (dopng) {
    if (is.null(dir)) {
      stop("Directory needs to be set.")
    }
    if (!file.exists(dir)) {
      stop(
        "The dir argument leads to a location",
        ",\ni.e., ",
        dir,
        ", that doesn't exist."
      )
    }

    plotdir <- file.path(dir, paste("map_plots", sep = ""))
    plotdir.isdir <- file.info(plotdir)$isdir
    if (is.na(plotdir.isdir) | !plotdir.isdir) {
      dir.create(plotdir)
    }
  }

  if (!dopng) {
    windows(width = 5, height = 7, record = TRUE)
  }

  ind <- dat$cpue_kg_km2 > 0
  pos.cat <- dat[ind, ]
  neg <- dat[dat[, "cpue_kg_km2"] == 0, ]
  mid <- as.numeric(stats::quantile(pos.cat[, "cpue_kg_km2"], 0.50))
  max.size <- 12

  color <- c("#fffa00", "#ffcc00", "#ff7700", "#B60000")

  igroup <- 1
  if (igroup %in% plot) {
    if (dopng) {
      pngfun(
        dir = plotdir,
        file = paste(
          ifelse(is.null(main), "", paste0(main, "_")),
          "CPUE_Map.png"
        ),
        h = 7,
        w = 5
      )
    }

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
      draw_USEEZ(c(-126, -117), c(32, 50)) +
      label_land() +
      label_axes() +
      theme(legend.position = "right")
    print(g)

    if (dopng) {
      dev.off()
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

    if (dopng) {
      pngfun(
        dir = plotdir,
        file = paste(
          ifelse(is.null(main), "", paste0(main, "_")),
          "CPUE_Map_Year.png"
        ),
        h = 7,
        w = 7
      )
    }

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
      draw_USEEZ(c(-126, -117), c(32, 50)) +
      label_axes() +
      theme(legend.position = "right") +
      facet_wrap(~Year, ncol = 6)
    print(h)

    if (dopng) {
      dev.off()
    }
  }
}
