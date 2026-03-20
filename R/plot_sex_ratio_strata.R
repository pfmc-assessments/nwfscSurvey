#' Function to plot sex ratio by strata
#'
#' @inheritParams pull_catch
#' @param data Data object with biological data from [pull_bio()] with a column
#' named `Sex` or `sex` is present.
#' @param comp_column_name The name of the column to create composition data for that
#'   must be a string. This column can be is used to determine whether to format the
#'   composition data for length or age compositions by looking for either age
#'   (e.g., `age_years`, `Age`, `age`, `best_age`) or length (e.g., `length`,
#'   `length_cm`, `Length`, `Length_cm`) in the comp_column_name. The comp_column_name
#'   is not case sensitive.The default is `length_cm`.
#' @param strata_vars Column names in `data` to define the stratas by.  The
#'   default columns are `Depth_m` or `depth_m` and `Latitude_dd` or `latitude_dd`.
#' @param strata_df Strata dataframe created by [create_strata()]
#' @param main Unique string that will be added to the saved png name.  This can
#'   be useful when plotting data for various data sets separately.
#' @param bin_width Width to bin the data by.  The default is 2 cm for lengths
#'   and 1 for ages.
#' @param width,height Numeric values for the figure width and height in
#'   inches. The defaults are 7 by 7 inches.
#'
#' @author Chantel Wetzel
#' @family plot_
#' @export
#' @seealso
#' \code{\link{strata_factors}}
#' \code{\link{create_strata}}
#'
plot_sex_ratio_strata <- function(
  data,
  strata_df,
  dir = NULL,
  comp_column_name = "length_cm",
  strata_vars = c("depth_m", "latitude_dd"),
  bin_width = ifelse(comp_column_name == "length_cm", 2, 1),
  main = NULL,
  height = 7,
  width = 7
) {
  plotdir <- file.path(dir)
  check_dir(dir = plotdir)
  main_ <- ifelse(is.null(main), "", paste0(main, "_"))
  if (!is.null(dir)) {
    filename <- paste0(main_, data.type, "_sex_ratio_strata.png")
  }

  data_mod <- data |>
    dplyr::rename_all(tolower) |>
    dplyr::mutate(
      stratum = NA
    )
  comp_column_name <- tolower(comp_column_name)
  if (!comp_column_name %in% colnames(data_mod)) {
    cli::cli_abort(
      "The following column is missing in the data: {comp_column_name}."
    )
  }
  data_mod$column_to_use <- data_mod[, comp_column_name]
  data_mod$stratum <- strata_factors(
    data = data_mod,
    strata_vars = strata_vars,
    strata_df = strata_df
  )
  data_mod$bin <- plyr::round_any(
    data_mod[, comp_column_name],
    bin_width,
    floor
  )
  binned_data <- data_mod |>
    dplyr::filter(!is.na(stratum)) |>
    dplyr::group_by(stratum) |>
    dplyr::count(bin, sex) |>
    dplyr::group_by(stratum) |>
    dplyr::mutate(proportion = n / sum(n)) |>
    dplyr::rename(Sex = sex)

  axis_name <- dplyr::case_when(
    tolower(comp_column_name) == "length_cm" ~ "Length (cm)",
    .default = "Age (years)"
  )
  colors <- viridis::viridis(n = 3)
  p <- ggplot2::ggplot(
    binned_data,
    ggplot2::aes(x = bin, y = proportion, fill = Sex)
  ) +
    ggplot2::geom_bar(position = "fill", stat = "identity") +
    ggplot2::geom_hline(yintercept = 0.50, col = "white", lwd = 1) +
    ggplot2::scale_fill_manual(
      values = c("F" = colors[1], "M" = colors[2], "U" = colors[3])
    ) +
    ggplot2::labs(y = "Proportion by Sex", x = axis_name) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        size = 1
      ),
      panel.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 15),
      axis.title.x = ggplot2::element_text(size = 15),
      axis.title.y = ggplot2::element_text(size = 15),
      axis.text.x = ggplot2::element_text(size = 15),
      axis.text.y = ggplot2::element_text(size = 15)
    ) +
    ggplot2::facet_wrap("stratum")

  if (!is.null(dir)) {
    ggplot2::ggsave(
      plot = p,
      filename = file.path(
        dir,
        filename
      ),
      width = width,
      height = height,
      units = "in"
    )
  } else {
    return(p)
  }
}
