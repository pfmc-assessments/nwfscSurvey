#' Plot variability of length at age
#'
#'
#' @param data A data frame of length-composition data returned from
#'   [pull_bio()].
#' @param age_bins Vector of integers to bin age data.Values above or below the minimum or maximum
#'   values in the vector are grouped into the first size or plus group size, respectively.
#' @param dir Defaults to NULL (plot made, but not saved to file). Can alternatively be a string filename
#' by year and sex.
#' @param main Name that will be used to name the saved png
#' @param two_sex Logical to indicate if plot by sex. Default is TRUE and will only plot females and males.
#' @param width,height Numeric values for the figure width and height in
#'   inches. The defaults are 7 by 7 inches.
#'
#' @author Chantel Wetzel
#' @family plot_
#' @export

plot_var_length_at_age <- function(
  data,
  age_bins,
  dir = NULL,
  main = NULL,
  two_sex = TRUE,
  height = 7,
  width = 7
) {
  plotdir <- file.path(dir)
  check_dir(dir = plotdir)
  if (!is.null(main) & !is.null(dir)) {
    main <- paste0(main, "_")
  }

  tolower_data <- data |>
    dplyr::rename_all(tolower) |>
    dplyr::filter(!is.na(length_cm), !is.na(age_years)) |>
    dplyr::mutate(sex = codify_sex(sex))

  required_columns <- c("length_cm", "age_years", "sex")

  if (any(!required_columns %in% colnames(tolower_data))) {
    cli::cli_abort(
      "Missing column in the data object: must include a column called length_cm, age_years, and sex."
    )
  }
  if (sum(!is.na(tolower_data[, "age_years"])) == 0) {
    cli::cli_abort(
      "All values in the age_years column are NA.  Must have age data available."
    )
  }

  if (two_sex) {
    tolower_data <- tolower_data |> dplyr::filter(sex != "U")
  } else {
    tolower_data[, "sex"] <- "U"
  }

  bins <- c(-999, age_bins, Inf)
  tolower_data[, "bin"] <- bins[findInterval(
    as.numeric(tolower_data[, "age_years"]),
    bins,
    all.inside = TRUE
  )]
  tolower_data <- tolower_data |>
    dplyr::mutate(
      bin = dplyr::case_when(bin == -999 ~ min(age_bins), .default = bin)
    )

  variation <- tolower_data |>
    dplyr::group_by(sex, bin) |>
    dplyr::summarise(
      mean = mean(length_cm),
      sd = sd(length_cm),
      cv = (sd / mean),
      .groups = 'drop'
    ) |>
    dplyr::rename(Sex = sex)

  p1 <- ggplot2::ggplot(variation) +
    ggplot2::geom_point(
      ggplot2::aes(x = bin, y = sd, color = Sex, pch = Sex),
      size = 2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = bin, y = sd, color = Sex),
      linetype = 2
    ) +
    #ggokabeito::scale_color_okabe_ito() +
    ggplot2::scale_color_viridis_d(begin = 0.0, end = 0.5) +
    ggplot2::xlab("Age (years)") +
    ggplot2::ylab("SD of Length-at-Age") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 1
      ),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14)
    )

  p2 <- ggplot2::ggplot(variation) +
    ggplot2::geom_point(
      ggplot2::aes(x = bin, y = cv, color = Sex, pch = Sex),
      size = 2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(x = bin, y = cv, color = Sex),
      linetype = 2
    ) +
    #ggokabeito::scale_color_okabe_ito() +
    ggplot2::scale_color_viridis_d(begin = 0.0, end = 0.5) +
    ggplot2::xlab("Age (years)") +
    ggplot2::ylab("CV of Length-at-Age") +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        linewidth = 1
      ),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 14)
    )

  if (!is.null(dir)) {
    ggsave(
      plot = cowplot::plot_grid(p1, p2, nrow = 2),
      filename = file.path(
        dir,
        paste0(main, "length_at_age_variation.png")
      ),
      width = width,
      height = height,
      units = "in"
    )
  } else {
    return(cowplot::plot_grid(p1, p2, nrow = 2))
  }
}
