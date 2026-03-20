#' This function plots cpue and length by latitude and depth
#'
#' @inheritParams pull_catch
#' @param data Data catch file pulled using [pull_catch()]
#' @param plot A vector of integers to specify which plots to return. The
#'   default is to print or save all figures, i.e., `plot = 1:3`. Integers
#'   correspond to the following figures:
#'   1. log(CPUE) by depth & log(CPUE) by latitude
#'   2. log(CPUE) by latitude and year
#'   3. log(CPUE) by depth and year
#' @param width,height Numeric values for the figure width and height in
#'   inches. The defaults are 7 by 7 inches.
#'
#' @import ggplot2
#' @import cowplot
#'
#' @author Chantel Wetzel
#' @export
#' @family plot_
#'
plot_cpue <- function(
  data,
  dir = NULL,
  plot = 1:3,
  width = 7,
  height = 7
) {
  plotdir <- file.path(dir)
  check_dir(dir = plotdir)

  data$log_cpue <- log(data$cpue_kg_km2)
  pos <- data$cpue_kg_km2 != 0
  size_adj <- 100 / floor(sum(pos))

  # plot 1 - marginal log(cpue) by depth and latitude
  if (1 %in% plot) {
    # log(cpue) by depth
    cd <- ggplot2::ggplot(
      data[pos, ],
      ggplot2::aes(x = Depth_m, y = log_cpue)
    ) +
      ggplot2::geom_point(
        ggplot2::aes(size = log_cpue / size_adj),
        alpha = 0.4,
        shape = 21
      ) +
      ggplot2::labs(
        x = "Depth (m)",
        y = "ln(CPUE)",
        size = "ln(CPUE)"
      ) +
      ggplot2::geom_smooth(
        method = "loess",
        formula = y ~ x,
        color = "darkgrey",
        lwd = 2
      ) +
      ggplot2::scale_x_continuous(n.breaks = 7) +
      ggplot2::scale_y_continuous(n.breaks = 7) +
      ggplot2::theme(
        legend.key = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(colour = "black", size = 12),
        axis.text.y = ggplot2::element_text(colour = "black", size = 11),
        legend.text = ggplot2::element_text(size = 10, colour = "black"),
        legend.title = ggplot2::element_text(size = 12),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(fill = NA),
        legend.position = "right"
      ) +
      ggplot2::guides(size = "legend", color = "none", fill = "none")

    # log(cpue) by latitude
    cl <- ggplot2::ggplot(
      data[pos, ],
      ggplot2::aes(x = Latitude_dd, y = log_cpue)
    ) +
      ggplot2::geom_point(
        ggplot2::aes(size = log_cpue / size_adj),
        alpha = 0.4,
        shape = 21
      ) +
      ggplot2::geom_smooth(
        method = "loess",
        formula = y ~ x,
        color = "darkgrey",
        lwd = 2
      ) +
      ggplot2::labs(
        x = "Latitude",
        y = "ln(CPUE)",
        size = "ln(CPUE)"
      ) +
      ggplot2::scale_x_continuous(n.breaks = 7) +
      ggplot2::scale_y_continuous(n.breaks = 7) +
      theme(
        legend.key = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(colour = "black", size = 12),
        axis.text.y = ggplot2::element_text(colour = "black", size = 11),
        legend.text = ggplot2::element_text(size = 10, colour = "black"),
        legend.title = ggplot2::element_text(size = 12),
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(fill = NA),
        legend.position = "right"
      )
    if (!is.null(dir)) {
      filename <- f
      ggplot2::ggsave(
        plot = cl,
        filename = ile.path(
          dir,
          paste0("cpue_by_lat_depth.png")
        ),
        height = height,
        width = width,
        units = "in"
      )
    }
  }

  # plot 2 - log(cpue) by latitude and year
  if (2 %in% plot) {
    cly <- ggplot2::ggplot(
      data[pos, ],
      ggplot2::aes(x = Latitude_dd, y = log_cpue)
    ) +
      ggplot2::geom_point(
        ggplot2::aes(size = log_cpue / (100 * size_adj)),
        alpha = 0.4,
        shape = 21
      ) +
      ggplot2::facet_wrap(facets = "Year") +
      ggplot2::geom_smooth(
        method = "loess",
        formula = y ~ x,
        color = "darkgrey",
        lwd = 2
      ) +
      ggplot2::labs(
        x = "Latitude",
        y = "ln(CPUE)",
        size = "ln(CPUE)"
      ) +
      ggplot2::guides(size = "legend", color = "none", fill = "none") +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(fill = NA)
      )
    if (!is.null(dir)) {
      ggplot2::ggsave(
        plot = cly,
        filename = file.path(
          dir,
          paste0("cpue_by_year_lat.png")
        ),
        height = height,
        width = width,
        units = "in"
      )
    }
  }

  # plot 3 - log(cpue) by depth and year
  if (3 %in% plot) {
    cdy <- ggplot2::ggplot(
      data[pos, ],
      ggplot2::aes(x = Depth_m, y = log_cpue)
    ) +
      ggplot2::geom_point(
        ggplot2::aes(size = log_cpue / (100 * size_adj)),
        alpha = 0.40,
        shape = 21
      ) +
      ggplot2::facet_wrap(facets = "Year") +
      ggplot2::geom_smooth(
        method = "loess",
        formula = y ~ x,
        color = "darkgrey",
        lwd = 2
      ) +
      ggplot2::labs(
        x = "Depth (m)",
        y = "ln(CPUE)",
        size = "ln(CPUE)"
      ) +
      ggplot2::guides(size = "legend", color = "none", fill = "none") +
      ggplot2::theme(
        panel.background = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(fill = NA)
      )
    if (!is.null(dir)) {
      ggplot2::ggsave(
        plot = cdy,
        filename = file.path(
          dir,
          paste0("cpue_by_year_depth.png")
        ),
        height = height,
        width = width,
        units = "in"
      )
    }
  }

  if (is.null(dir)) {
    if (length(plot) == 1) {
      if (plot == 1) {
        return(cowplot::plot_grid(cl, cd, nrow = 2))
      }
      if (plot == 2) {
        return(cly)
      }
      if (plot == 3) {
        return(cdy)
      }
    }
    if (length(plot) == 2) {
      if (sum(1:2 %in% plot) == 2) {
        return(list(cowplot::plot_grid(cl, cd, nrow = 2), cly))
      }
      if (sum(2:3 %in% plot) == 2) {
        return(list(cly, cdy))
      }
      if (sum(c(1, 3) %in% plot) == 2) {
        return(list(cowplot::plot_grid(ll, ld, nrow = 2), cdy))
      }
    }
    if (length(plot) == 3) {
      return(list(cowplot::plot_grid(cl, cd, nrow = 2), cly, cdy))
    }
  }
}
