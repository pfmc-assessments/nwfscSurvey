#' Plots the weight-at-length data and estiamtes
#'
#'
#' @param data Dataframe created by [pull_bio()]
#' @template dir
#' @param estimates Dataframe of weight-at-length estimates from [est_weight_length()].
#'   If passed to the function the estimated parameter values will be added to
#'   the plot. The default is `NULL` and will not add parameters to the plot.
#' @param col_length A numeric or character value specifying the column
#'   to use in `data` for length information. These lengths are assumed to
#'   be in centimeters. The default value is `lengthcm`, which is added
#'   to a data set automatically when running [cleanPacFIN()].
#' @param col_weight A numeric or character value specifying the column
#'   to use in `data` for weight information. These weights are assumed to
#'   be in kilograms The default value is `weightkg`, which is added
#'   to a data set automatically when running [cleanPacFIN()].
#'   Using kilograms is the default because Stock Synthesis assumes the
#'   weight-length parameters are calculated using centimeters and kilograms.
#'   The reported values are easily scaled to give you results in grams if
#'   you wish to have more standard parameter estimates.
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
plot_weight_length <- function(
    data,
    dir = NULL,
    estitmates = NULL,
    col_length = "length_cm",
    col_weight = "weight_kg",
    two_sex = TRUE,
    add_save_name = NULL,
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
      "weigth_at_length.png"
    )
  )

  colnames(data) <- tolower(colnames(data))
  stopifnotcolumn(data = data, string = col_length)
  stopifnotcolumn(data = data, string = col_weight)
  stopifnotcolumn(data = data, string = "sex")

  data[, "length_column"] <- data[, col_length]
  data[, "weight_column"] <- data[, col_weight]

  data_to_plot <- data |>
    dplyr::filter(
      !is.na(weight_column),
      weight_column > 0,
      !is.na(length_column),
      length_column > 0)

  xlims <- c(0, ceiling(max(data_to_plot[, "length_column"])))
  ylims <- c(0, max(data_to_plot[, "weight_column"]))

  if (!is.null(estimates)) {
    estimates[, "lmin"] <- 0
    estimates[, "lmax"] <- xlims[2]

    lines_to_plot <- estimates |>
      dplyr::group_by(sex) |>
      dplyr::reframe(
        plot_length = seq(lmin, lmax, 1),
        plot_weight = A * plot_length ^ B,
        a = A,
        b = B
      ) |>
      dplyr::mutate(
        sex = dplyr::case_match(sex, "all" ~ "U", "female" ~ "F", "male" ~ "M")
      )

    label <- lines_to_plot |>
      dplyr::mutate(
        max_y = quantile(plot_weight, 0.95),
        multiplier = ifelse(sex == "F", 1, ifelse(sex == "M", 0.90, 0.80))
      ) |>
      dplyr::group_by(sex) |>
      dplyr::summarize(
        label = paste0("a = ", format(unique(a), digits = 3, scientific = TRUE), "; ", paste0("b = ", round(unique(b), 2))),
        x = quantile(plot_length, 0.30),
        y = unique(max_y) * unique(multiplier)
      )
  }

  if (two_sex) {
    data_to_plot <- data_to_plot |> dplyr::filter(sex != "U")
    lines_to_plot <- lines_to_plot |> dplyr::filter(sex != "U")
    label <- label |> dplyr::filter(sex != "U")
    colors <- line_colors <- c("#414487FF", "#22A884FF")
    point_alpha <- 0.10
  } else {
    data_to_plot[, "sex"] <- "U"
    lines_to_plot <- lines_to_plot |> dplyr::filter(sex == "U")
    label <- label |> dplyr::filter(sex == "U")
    colors <- "grey"
    point_alpha <- 0.10
    line_colors <- "black"
  }

  p1 <- ggplot2::ggplot(data_to_plot) +
    ggplot2::geom_point(aes(x = length_column, y = weight_column, color = sex), alpha = point_alpha, size = 1)+
    ggplot2::ylab("Weight (kg)") +
    ggplot2::xlab("Length (cm)") +
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
        ggplot2::geom_line(data = lines_to_plot,
                           ggplot2::aes(x = plot_length, y = plot_weight, linetype = sex, color = sex), linewidth = 1.0) +
                ggplot2::guides(color = guide_legend(override.aes = list(alpha = 1, size = 3, linetype = 0)), shape = "none")
    } else {
      p1 <- p1 +
        ggplot2::geom_text(data = label, show.legend = FALSE, ggplot2::aes(x = x, y = y, label = label), color = line_colors) +
        ggplot2::geom_line(data = lines_to_plot,
                           ggplot2::aes(x = plot_length, y = plot_weight, linetype = sex), color = line_colors, linewidth = 1.0) +
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
