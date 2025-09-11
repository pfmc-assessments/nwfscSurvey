#' Plots the design based biomass estimates with confidence intervals
#'
#' @details
#' Plots both the design based biomass estimates by year with confidence intervals
#' and the design based biomass estimates by year and strata with no confidence
#' intervals.
#'
#' @param data List of design based biomass estimates created by the [get_design_based()]
#' @template dir
#' @param add_save_name Option to add text to a saved figure name. This option
#'   can be useful if creating plots across multiple species and saving them
#'   into a single folder. The default is `NULL`. Note that the biomass estimate,
#'   i.e., annual or strata, are already included in the saved name
#'   so no need to add those here.
#' @param plot A vector of integers to specify which plots you would like. The
#'   default is to print or save both figures, i.e., `plot = 1:2`. Integers
#'   correspond to the following figures:
#'   1. Design based index by year.
#'   2. Design based index by year and strata.
#' @param width,height Numeric values for the figure width and height in
#'   inches. The defaults are 7 by 7 inches.
#' @param dpi The resolution to apply when saving figures.  Lower resolution values
#'    can reduce file size which can be helpful when creating large documents
#'    with many figures. The default is 300.
#'
#' @author Chantel Wetzel
#' @import ggplot2
#' @export
#'
plot_index <- function(
    data,
    dir = NULL,
    add_save_name = NULL,
    plot = 1:2,
    height = 7,
    width = 7,
    dpi = 300) {
  plotdir <- file.path(dir, "plots")
  check_dir(plotdir)
  plot_names <- file.path(
    plotdir,
    paste0(
      add_save_name,
      ifelse(test = is.null(add_save_name), yes = "", no = "_"),
      c(
        "design_based_index.png",
        "design_based_index_strata.png"
      )
    )
  )

  if (is.list(data)) {
    if ("biomass" %in% names(data)) {
      data_1 <- data$biomass
    } else {
      warning("Biomass not present in the data list. Annual biomass will not be plotted.")
      plot <- plot[plot != 1]
    }
    if ("biomass_by_strata" %in% names(data)) {
      data_2 <- data$biomass_by_strata
    } else {
      warning("Biomass by strata not present in the data list. Biomass by strata will not be plotted.")
      plot <- plot[plot != 2]
    }
  } else {
    stop("Data should be a list created by [get_design_based()].")
  }

  igroup <- 1
  if (igroup %in% plot) {
    gg <- ggplot2::ggplot(
      data = data_1,
      ggplot2::aes(
        x = year,
        y = est,
      )
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_line(lty = 2) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = lwr, ymax = upr)
      ) +
      ggplot2::theme_bw() +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::xlab("Year") +
      ggplot2::ylab("Design Based Index (mt)") +
      ggplot2::expand_limits(y = 0)

    if (!is.null(dir)) {
      ggplot2::ggsave(
        filename = plot_names[1], plot = gg,
        height = height, width = width, units = "in", dpi = dpi
      )
    } else {
      print(gg)
    }
  }

  igroup <- 2
  if (igroup %in% plot) {
    g2 <- ggplot2::ggplot(
      data = data_2,
      ggplot2::aes(
        x = year,
        y = est,
      )
    ) +
      ggplot2::geom_point() +
      ggplot2::geom_line(lty = 2) +
      ggplot2::theme_bw() +
      ggplot2::scale_colour_viridis_d() +
      ggplot2::xlab("Year") +
      ggplot2::ylab("Design Based Index by Strata (mt)") +
      ggplot2::expand_limits(y = 0) +
      ggplot2::facet_wrap(
        facets = "stratum", scales = "free", ncol = 2
      )

    if (!is.null(dir)) {
      ggplot2::ggsave(
        filename = plot_names[2], plot = g2,
        height = height, width = width, units = "in", dpi = dpi
      )
    } else {
      print(g2)
    }
  }
}
