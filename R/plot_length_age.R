#' Plots the length-at-age data and estimates
#'
#'
#' @param data Data frame created by [pull_bio()]
#' @template dir
#' @param estimates Data frame of age-at-length estimates from [est_vbgrowth()].
#'   If passed to the function the estimated parameter values will be added to
#'   the plot. The default is `NULL` and will not add parameters to the plot.
#' @param col_length A numeric or character value specifying the column
#'   to use in `data` for length information. These lengths are assumed to
#'   be in centimeters. The default value is `length_cm`.
#' @param col_age A numeric or character value specifying the column
#'   to use in `data` for age information. These ages are assumed to
#'   be in years. The default value is `age`.
#' @param two_sex Default TRUE. If TRUE the sexed data will be plotted and if
#'   FALSE all data will be plotted as unsexed. If `estimates` are passed to the
#'   function then the corresponding parameter estimates will be plotted.
#' @param add_save_name Option to add text to a saved figure name. This option
#'   can be useful if creating plots across multiple species and saving them
#'   into a single folder. The default is `NULL`. Note that the biomass estimate,
#'   i.e., annual or strata, are already included in the saved name
#'   so no need to add those here.
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
plot_length_age <- function(
  data,
  dir = NULL,
  col_length = "length_cm",
  col_age = "age",
  estimates = NULL,
  two_sex = TRUE,
  add_save_name = NULL,
  height = 7,
  width = 7,
  dpi = 300
) {
  plotdir <- file.path(dir, "plots")
  check_dir(plotdir)
  plot_names <- file.path(
    plotdir,
    paste0(
      add_save_name,
      ifelse(test = is.null(add_save_name), yes = "", no = "_"),
      "length_at_age.png"
    )
  )

  colnames(data) <- tolower(colnames(data))
  stopifnotcolumn(data = data, string = col_length)
  stopifnotcolumn(data = data, string = col_age)
  stopifnotcolumn(data = data, string = "sex")

  data[, "length_column"] <- data[, col_length]
  data[, "age_column"] <- data[, col_age]

  data_to_plot <- data |>
    dplyr::filter(
      !is.na(age_column),
      age_column > 0,
      !is.na(length_column),
      length_column > 0
    )

  ylims <- c(0, ceiling(max(data_to_plot[, "length_column"])))
  xlims <- c(0, max(data_to_plot[, "age_column"]))

  para <- as.data.frame(do.call(rbind, estimates[1:3]))
  para[, "sex"] <- c("F", "M", "U")
  para[, "amin"] <- 0
  para[, "amax"] <- xlims[2]

  lines_to_plot <- para |>
    dplyr::group_by(sex) |>
    dplyr::reframe(
      k = K,
      Linf = Linf,
      L0 = L0,
      age = seq(amin, amax, 1),
      length_cm = Linf + (L0 - Linf) * exp(-K * age)
    )
  label <- lines_to_plot |>
    dplyr::mutate(
      max_x = quantile(age, 0.60),
      multiplier = ifelse(sex == "M", 0.10, 0.20)
    ) |>
    dplyr::group_by(sex) |>
    dplyr::summarize(
      label = paste0(
        "k = ", round(unique(k), 2), "; ",
        paste0("Lmin = ", round(unique(L0), 1)), "; ",
        paste0("Linf = ", round(unique(Linf), 1))
      ),
      x = unique(max_x),
      y = unique(max(length_cm)) * unique(multiplier)
    )

  if (two_sex) {
    data_to_plot <- data_to_plot |> dplyr::filter(sex != "U")
    label <- label |> dplyr::filter(sex != "U")
    lines_to_plot <- lines_to_plot |> dplyr::filter(sex != "U")
    colors <- line_colors <- c("#414487FF", "#22A884FF")
    point_alpha <- 0.10
  } else {
    data_to_plot[, "sex"] <- "U"
    label <- label |> dplyr::filter(sex == "U")
    lines_to_plot <- lines_to_plot |> dplyr::filter(sex == "U")
    colors <- "darkgrey"
    point_alpha <- 0.10
    line_colors <- "black"
  }

  p1 <- ggplot2::ggplot(data_to_plot) +
    ggplot2::geom_point(aes(y = length_column, x = age_column, color = sex), alpha = point_alpha, size = 1) +
    ggplot2::xlab("Age (years)") +
    ggplot2::ylab("Length (cm)") +
    ggplot2::xlim(xlims[1], xlims[2]) +
    ggplot2::ylim(ylims[1], ylims[2]) +
    ggplot2::theme_bw() +
    ggplot2::scale_color_manual(name = "Sex", values = colors) +
    ggplot2::scale_fill_manual(name = "Sex", values = colors) +
    ggplot2::guides(color = guide_legend(override.aes = list(alpha = 1, size = 3)))

  if (!is.null(estimates)) {
    if (two_sex) {
      p1 <- p1 +
        ggplot2::geom_text(data = label, show.legend = FALSE, ggplot2::aes(x = x, y = y, label = label, color = sex)) +
        ggplot2::geom_line(
          data = lines_to_plot,
          ggplot2::aes(y = length_cm, x = age, linetype = sex, color = sex), linewidth = 1.0
        ) +
        ggplot2::guides(color = guide_legend(override.aes = list(alpha = 1, size = 3, linetype = 0)), shape = "none")
    } else {
      p1 <- p1 +
        ggplot2::geom_text(data = label, show.legend = FALSE, ggplot2::aes(x = x, y = y, label = label), color = line_colors) +
        ggplot2::geom_line(
          data = lines_to_plot,
          ggplot2::aes(y = length_cm, x = age, linetype = sex), color = line_colors, linewidth = 1.0
        ) +
        ggplot2::guides(color = guide_legend(override.aes = list(alpha = 1, size = 3, linetype = 0)), shape = "none")
    }
  }

  if (!is.null(dir)) {
    ggplot2::ggsave(
      filename = plot_names, plot = p1,
      height = height, width = width, units = "in", dpi = dpi
    )
  } else {
    print(p1)
  }
}
