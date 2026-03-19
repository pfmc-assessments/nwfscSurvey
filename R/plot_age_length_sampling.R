#' Plot the representativeness of age sampling based on lengths
#'
#' @param data A data frame of composition data returned from
#'   [pull_bio()].
#' @param xlim Deprecated as of v.2.8.1. x limits for plot, defaults to c(0,120)
#' @param ylim Deprecated as of v.2.8.1. y limits for plot, defaults to (0, 0.049)
#' @param plot A vector of integers to specify which plots to return. The
#'   default is to print or save all figures, i.e., `plot = 1:2`. Integers
#'   correspond to the following figures:
#'   1. Compare distribution of all lengths and lengths from aged fish across all years
#'   2. Compare distribution of all lengths and lengths from aged fish across by year
#' @param dir Defaults to NULL (plot made, but not saved to file). Can alternatively be a string filename
#' by year and sex.
#' @param width,height Numeric values for the figure width and height in
#'   inches. The defaults are 7 by 7 inches.
#'
#' @author Chantel Wetzel
#' @family plot_
#' @export
plot_age_length_sampling <- function(
  data,
  dir = NULL,
  plot = 1:2,
  height = 7,
  width = 7,
  xlim = lifecycle::deprecated(),
  ylim = lifecycle::deprecated()
) {
  if (lifecycle::is_present(xlim)) {
    lifecycle::deprecate_warn(
      when = "2.8.1",
      what = "nwfscSurvey::plot_age_length_sampling(xlim =)"
    )
  }
  if (lifecycle::is_present(ylim)) {
    lifecycle::deprecate_warn(
      when = "2.8.1",
      what = "nwfscSurvey::plot_age_length_sampling(ylim =)"
    )
  }
  if (!is.null(dir)) {
    filename <- file.path(dir, "age_length_comparison.png")
  }
  plotdir <- file.path(dir)
  check_dir(dir = plotdir)
  plot_names <- file.path(
    plotdir,
    c(
      "length_age_distribution_comparison.png",
      "length_age_distribution_comparison_by_year.png"
    )
  )

  data_tolower <- data |> dplyr::rename_all(tolower)
  if (any(c("length_cm", "age_years", "year") %in% colnames(data_tolower))) {
    cli::cli_abort(
      "Missing column in the data object: must include a column called length_cm, age_years, and year."
    )
  }
  if (sum(!is.na(data_tolower[, "age_years"])) == 0) {
    cli::cli_abort(
      "All values in the age_years column are NA.  Must have age data available."
    )
  }

  length_data <- data_tolower |>
    dplyr::rename_all(tolower) |>
    dplyr::filter(!is.na(length_cm)) |>
    dplyr::mutate(Type = "All Fish")

  age_data <- data_tolower |>
    dplyr::rename_all(tolower) |>
    dplyr::filter(!is.na(age_years), !is.na(length_cm)) |>
    dplyr::mutate(Type = "Aged Fish")
  years_to_keep <- unique(length_data[, "year"])[which(
    unique(length_data[, "year"]) %in% unique(age_data[, "year"])
  )]

  both_data_types <- dplyr::bind_rows(
    length_data,
    age_data
  ) |>
    dplyr::filter(year %in% years_to_keep)

  if (1 %in% plot) {
    p1 <- ggplot2::ggplot(
      both_data_types,
      ggplot2::aes(x = length_cm, color = Type, linetype = Type)
    ) +
      ggplot2::geom_density(linewidth = 1) +
      ggplot2::scale_color_viridis_d(begin = 0.0, end = 0.5) +
      ggplot2::ylab("Density") +
      ggplot2::xlab("Length (cm)") +
      ggplot2::theme_bw()

    if (!is.null(dir)) {
      ggsave(
        filename = plot_names[1],
        plot = p1,
        width = width,
        height = height,
        units = "in"
      )
    } else {
      print(p1)
    }
  }

  if (2 %in% plot) {
    p2 <- ggplot2::ggplot(
      both_data_types,
      ggplot2::aes(x = length_cm, color = Type, linetype = Type)
    ) +
      ggplot2::geom_density(linewidth = 1) +
      ggplot2::scale_color_viridis_d(begin = 0.0, end = 0.5) +
      ggplot2::ylab("Density") +
      ggplot2::xlab("Length (cm)") +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(facets = "year")

    if (!is.null(dir)) {
      ggsave(
        filename = plot_names[2],
        plot = p2,
        width = width,
        height = height,
        units = "in"
      )
    } else {
      print(p2)
    }
  }
}
