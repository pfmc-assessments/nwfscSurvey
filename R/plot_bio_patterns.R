#' This function plots length by latitude and depth
#'
#' @param data Biological data frame from [pull_bio()]
#' @param dir Directory where output will be saved. The directory where the file should be saved.
#' If dir = NULL no output will be saved.
#' @param col_name Option to switch between plotting lengths or ages.
#' Options are "Length_cm", "Width_cm", or "Age".
#' @param plot A vector of integers to specify which plots you would like. The
#'   default is to print or save both figures, i.e., `plot = 1:3`. Integers
#'   correspond to the following figures:
#'   1. length/age by latitude and depth
#'   2. length/age by depth and year
#'   3. length/age by lat and year
#' @param width,height Numeric values for the figure width and height in
#'   inches. The defaults are 10 by 7 inches.
#'
#' @import ggplot2
#' @import cowplot
#'
#' @author Chantel Wetzel
#' @export
#'
#'
plot_bio_patterns <- function(
  data,
  dir = NULL,
  col_name = "Length_cm",
  plot = 1:3,
  width = 7,
  height = 7
) {
  round_any <- function(x, accuracy, f = round) {
    f(x / accuracy) * accuracy
  }

  plotdir <- file.path(dir)
  check_dir(dir = plotdir)

  lab_name <- col_name
  if (col_name == "Length_cm") {
    lab_name <- "Length (cm)"
    data$x <- data$Length_cm
  }
  if (col_name == "Width_cm") {
    lab_name <- "Width (cm)"
    data$x <- data$Width_cm
  }
  if (col_name == "Age") {
    lab_name <- "Age (yrs)"
    data$x <- data$Age
  }

  alpha_set <- 0.30
  alpha_by_year <- 0.40
  if (dim(data)[1] > 10000) {
    alpha_set <- 0.05
    alpha_by_year <- 0.20
    if (dim(data)[1] > 50000) {
      alpha_set <- 0.01
      alpha_by_year <- 0.10
    }
  }
  bin_size <- ifelse(
    max(data$Depth_m) - min(data$Depth_m) > 500,
    100,
    ifelse(max(data$Depth_m) - min(data$Depth_m) > 250, 50, 25)
  )
  data <- data |>
    dplyr::mutate(
      depth_bin = round_any(Depth_m, bin_size, f = floor),
      lat_bin = round_any(Latitude_dd, 0.25)
    )

  # Length by depth for each sex
  ld <- ggplot2::ggplot(
    data,
    aes(x = Depth_m, y = x, color = Sex)
  ) +
    geom_point(
      aes(fill = Sex, colour = Sex, shape = Sex),
      alpha = alpha_set,
      size = 1
    ) +
    stat_summary(
      aes(y = x, x = depth_bin, colour = Sex, linetype = Sex),
      fun = mean,
      geom = "line",
      lwd = 1,
      alpha = 1
    ) +
    scale_fill_manual(
      values = c("F" = "#440154FF", "M" = "#21908CFF", "U" = "#FDE725FF")
    ) +
    scale_color_manual(
      values = c("F" = "#440154FF", "M" = "#21908CFF", "U" = "#FDE725FF")
    ) +
    labs(x = "Depth (m)", y = lab_name) +
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

  # Length by latitude
  ll <- ggplot2::ggplot(data, aes(x = Latitude_dd, y = x)) +
    geom_point(
      aes(fill = Sex, colour = Sex, shape = Sex),
      alpha = alpha_set,
      size = 1
    ) +
    stat_summary(
      aes(y = x, x = lat_bin, colour = Sex, linetype = Sex),
      fun = mean,
      geom = "line",
      lwd = 1,
      alpha = 1
    ) +
    scale_fill_manual(
      values = c("F" = "#440154FF", "M" = "#21908CFF", "U" = "#FDE725FF")
    ) +
    scale_color_manual(
      values = c("F" = "#440154FF", "M" = "#21908CFF", "U" = "#FDE725FF")
    ) +
    labs(x = "Latitude", y = lab_name) +
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

  # Length by latitude and sex by year
  lly <- ggplot2::ggplot(data, aes(x = Latitude_dd, y = x)) +
    geom_point(
      aes(fill = Sex, colour = Sex, shape = Sex),
      alpha = alpha_by_year,
      size = 1
    ) +
    facet_wrap(facets = "Year") +
    scale_fill_manual(
      values = c("F" = "#440154FF", "M" = "#21908CFF", "U" = "#FDE725FF")
    ) +
    scale_color_manual(
      values = c("F" = "#440154FF", "M" = "#21908CFF", "U" = "#FDE725FF")
    ) +
    labs(x = "Latitude", y = lab_name) +
    theme(
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA)
    )

  # Length by depth and sex by year
  ldy <- ggplot2::ggplot(data, aes(x = Depth_m, y = x)) +
    geom_point(
      aes(fill = Sex, colour = Sex, shape = Sex),
      alpha = alpha_by_year,
      size = 1
    ) +
    facet_wrap(facets = "Year") +
    scale_fill_manual(
      values = c("F" = "#440154FF", "M" = "#21908CFF", "U" = "#FDE725FF")
    ) +
    scale_color_manual(
      values = c("F" = "#440154FF", "M" = "#21908CFF", "U" = "#FDE725FF")
    ) +
    labs(x = "Depth (m)", y = lab_name) +
    theme(
      panel.background = element_blank(),
      panel.border = element_rect(fill = NA)
    )

  # plot 1
  if (1 %in% plot) {
    if (!is.null(dir)) {
      ggsave(
        plot = cowplot::plot_grid(ll, ld, nrow = 2),
        filename = file.path(
          dir,
          paste0(col_name, "_by_lat_depth.png")
        ),
        width = width,
        height = height,
        units = "in"
      )
    }
  }

  # plot 2
  if (2 %in% plot) {
    if (!is.null(dir)) {
      ggsave(
        plot = ldy,
        filename = file.path(
          dir,
          paste0(col_name, "_by_year_depth.png")
        ),
        width = width + 3,
        height = height + 3,
        units = "in"
      )
    }
  }

  # plot 3
  if (3 %in% plot) {
    if (!is.null(dir)) {
      ggsave(
        plot = lly,
        filename = file.path(
          dir,
          paste0(col_name, "_by_year_lat.png")
        ),
        width = width + 3,
        height = height + 3,
        units = "in"
      )
    }
  }

  if (is.null(dir)) {
    if (length(plot) == 1) {
      if (plot == 1) {
        return(cowplot::plot_grid(ll, ld, nrow = 2))
      }
      if (plot == 2) {
        return(ldy)
      }
      if (plot == 3) {
        return(lly)
      }
    }
    if (length(plot) == 2) {
      if (sum(1:2 %in% plot) == 2) {
        return(list(cowplot::plot_grid(ll, ld, nrow = 2), ldy))
      }
      if (sum(2:3 %in% plot) == 2) {
        return(list(ldy, lly))
      }
      if (sum(c(1, 3) %in% plot) == 2) {
        return(list(cowplot::plot_grid(ll, ld, nrow = 2), lly))
      }
    }
    if (length(plot) == 3) {
      return(list(cowplot::plot_grid(ll, ld, nrow = 2), ldy, lly))
    }
  }
}
