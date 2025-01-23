#' This function plots cpue and length by latitude and depth
#'
#' @template dir
#' @param catch Data catch file pulled using [pull_catch()]
#' @param plot A vector of integers to specify which plots to return. The
#'   default is to print or save all figures, i.e., `plot = 1:3`. Integers
#'   correspond to the following figures:
#'   1. log(CPUE) by depth & log(CPUE) by latitude
#'   2. log(CPUE) by latitude and year
#'   3. log(CPUE) by depth and year
#' @param width,height Numeric values for the figure width and height in
#'   inches. The defaults are 7 by 7 inches.
#' @param ... Additional arguments to [ggsave()]
#'
#' @import ggplot2
#' @import cowplot
#'
#' @author Chantel Wetzel
#' @export
#'
plot_cpue <- function(
    catch,
    dir = NULL,
    plot = 1:3,
    width = 7,
    height = 7,
    ...) {
  plotdir <- file.path(dir, "plots")
  check_dir(dir = plotdir)

  catch$log_cpue <- log(catch$cpue_kg_km2)
  pos <- catch$cpue_kg_km2 != 0
  size_adj <- 100 / floor(sum(pos))

  # ggsave arguments
  l <- as.list(substitute(...()))
  l$width <- width
  l$height <- height
  if (is.null(l$units)) l$units <- "in"
  if (is.null(l$device)) l$device <- "png" else l$device <- gsub("[^[:alnum:] ]", "", deparse(l$device))

  # plot 1 - marginal log(cpue) by depth and latitude
  if (1 %in% plot) {
    # log(cpue) by depth
    cd <- ggplot2::ggplot(catch[pos, ], aes(x = Depth_m, y = log_cpue)) +
      geom_point(aes(size = log_cpue / size_adj), fill = "darkorange", colour = "darkorange", alpha = 0.75, shape = 21) +
      labs(x = "Depth (m)", y = "ln(CPUE)", size = "ln(CPUE)", fill = "darkorange") +
      geom_smooth(method = "loess", color = "darkgrey", lwd = 2) +
      scale_x_continuous(n.breaks = 7) +
      scale_y_continuous(n.breaks = 7) +
      theme(
        legend.key = element_blank(),
        axis.text.x = element_text(colour = "black", size = 12),
        axis.text.y = element_text(colour = "black", size = 11),
        legend.text = element_text(size = 10, colour = "black"),
        legend.title = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        legend.position = "right"
      ) +
      guides(size = "legend", color = "none", fill = "none")

    # log(cpue) by latitude
    cl <- ggplot2::ggplot(catch[pos, ], aes(x = Latitude_dd, y = log_cpue)) +
      geom_point(aes(size = log_cpue / size_adj), fill = "darkorange", colour = "darkorange", alpha = 0.75, shape = 21) +
      geom_smooth(method = "loess", color = "darkgrey", lwd = 2) +
      labs(x = "Latitude", y = "ln(CPUE)", size = "ln(CPUE)", fill = "darkorange", colour = "darkorange", ) +
      scale_x_continuous(n.breaks = 7) +
      scale_y_continuous(n.breaks = 7) +
      theme(
        legend.key = element_blank(),
        axis.text.x = element_text(colour = "black", size = 12),
        axis.text.y = element_text(colour = "black", size = 11),
        legend.text = element_text(size = 10, colour = "black"),
        legend.title = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        legend.position = "right"
      )

    # plot 1
    print(cowplot::plot_grid(cl, cd, nrow = 2))
    if (!is.null(dir)) {
      l$filename <- file.path(dir, "plots", paste0("cpue_by_lat_depth.", l$device))
      do.call(ggsave, l)
    }
  }

  # plot 2 - log(cpue) by latitude and year
  if (2 %in% plot) {
    cly <- ggplot2::ggplot(catch[pos, ], aes(x = Latitude_dd, y = log_cpue)) +
      geom_point(aes(size = log_cpue / (100 * size_adj)), fill = "darkorange", colour = "darkorange", alpha = 0.75, shape = 21) +
      facet_wrap(facets = "Year") +
      geom_smooth(method = "loess", color = "darkgrey", lwd = 2) +
      labs(x = "Latitude", y = "ln(CPUE)", size = "ln(CPUE)", fill = "darkorange", colour = "darkorange", ) +
      guides(size = "legend", color = "none", fill = "none") +
      theme(
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA)
      )

    print(cly)
    if (!is.null(dir)) {
      l$filename <- file.path(dir, "plots", paste0("cpue_by_year_lat.", l$device))
      l2 <- l
      l2$width <- l2$width + 3
      l2$height <- l2$height + 3
      do.call(ggsave, l2)
    }
  }

  # plot 3 - log(cpue) by depth and year
  if (3 %in% plot) {
    cdy <- ggplot2::ggplot(catch[pos, ], aes(x = Depth_m, y = log_cpue)) +
      geom_point(aes(size = log_cpue / (100 * size_adj)), fill = "darkorange", colour = "darkorange", alpha = 0.75, shape = 21) +
      facet_wrap(facets = "Year") +
      geom_smooth(method = "loess", color = "darkgrey", lwd = 2) +
      labs(x = "Depth (m)", y = "ln(CPUE)", size = "ln(CPUE)", fill = "darkorange", colour = "darkorange") +
      guides(size = "legend", color = "none", fill = "none") +
      theme(
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA)
      )

    print(cdy)
    if (!is.null(dir)) {
      l$filename <- file.path(dir, "plots", paste0("cpue_by_year_depth.", l$device))
      l2 <- l
      l2$width <- l2$width + 3
      l2$height <- l2$height + 3
      do.call(ggsave, l2)
    }
  }
}
