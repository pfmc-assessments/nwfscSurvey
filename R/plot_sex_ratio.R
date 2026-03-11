#' Function to plot sex ratio data in a barplot form
#'
#' @template dir
#' @param data Data object with biological data from [pull_bio()] with a column
#' named `Sex` or `sex` is present.
#' @param comp_column_name The name of the column to create composition data for that
#'   must be a string. This column can be is used to determine whether to format the
#'   composition data for length or age compositions by looking for either age
#'   (e.g., `age_years`, `Age`, `age`, `best_age`) or length (e.g., `length`,
#'   `length_cm`, `Length`, `Length_cm`) in the comp_column_name. The comp_column_name
#'   is not case sensitive.The default is `length_cm`.
#' @param main Unique string that will be added to the saved png name.  This can
#'   be useful when plotting data for various data sets separately.
#' @param bin_width Width to bin the data by.  The default is 2 cm for lengths
#'   and 1 for ages.
#' @param width,height Numeric values for the figure width and height in
#'   inches. The defaults are 7 by 7 inches.
#'
#' @author Chantel Wetzel
#' @export
#'
plot_sex_ratio <- function(
  dir,
  data,
  comp_column_name = "length_cm",
  main = NULL,
  bin_width = ifelse(comp_column_name == "length_cm", 2, 1),
  width = 7,
  height = 7
) {
  plotdir <- file.path(dir)
  check_dir(dir = plotdir)
  main_ <- ifelse(is.null(main), "", paste0(main, "_"))
  if (!is.null(dir)) {
    filename = paste0(main_, data.type, "_sex_ratio.png")
  }

  data_tolower <- data |>
    dplyr::rename_all(tolower)
  comp_column_name <- tolower(comp_column_name)
  if (!comp_column_name %in% colnames(data)) {
    cli::cli_abort(
      "The following column is missing in the data: {comp_column_name}."
    )
  }
  data_tolower$column_to_use <- data_tolower[, tolower(comp_column_name)]
  binned_data <- data_tolower |>
    dplyr::mutate(bin = plyr::round_any(column_to_use, bin_width, floor)) |>
    dplyr::count(bin, sex) |>
    dplyr::mutate(proportion = n / sum(n)) |>
    dplyr::rename(Sex = sex)

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
    ggplot2::labs(y = "Proportion by Sex", x = axis.name) +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(
        colour = "black",
        fill = NA,
        size = 1
      ),
      panel.background = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(size = 15),
      axis.title.y = ggplot2::element_text(size = 15),
      axis.text.x = ggplot2::element_text(size = 15),
      axis.text.y = ggplot2::element_text(size = 15)
    )
  print(p)

  if (!is.null(dir)) {
    ggplot2::ggsave(
      filename = file.path(
        dir,
        filename
      ),
      width = width,
      height = height,
      units = "in"
    )
  }
}
