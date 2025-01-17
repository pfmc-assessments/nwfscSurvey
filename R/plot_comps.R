#' Plot frequency data as bubble plots
#'
#' @template dir
#' @param data Data file object created by [get_expanded_comps()] or [get_raw_comps()].
#' @param add_save_name Option to add text to a saved figure name. This option
#'   can be useful if creating plots across multiple species and saving them
#'   into a single folder. The default is `NULL`. Note that the data type,
#'   i.e., age or length, and sex type are already included in the saved name
#'   so no need to add those here.
#' @param plot A vector of integers to specify which plots you would like. The
#'   default is to print or save both figures, i.e., `plot = 1:2`. Integers
#'   correspond to the following figures:
#'   1. bubble plot of length-/age-composition data by year and sex and
#'   2. distribution by year of length-/age-compositions data similar to the
#'      r4ss figure.
#' @param add_0_ylim A logical, i.e., `TRUE`/`FALSE`, argument that specifies
#'   if the y axis should start at 0. If `FALSE`, the y axis will start at the
#'   minimum bin size used in data. The default is TRUE. This currently only
#'   pertains to plot 1, not plot 2.
#' @param width,height Numeric values for the figure width and height in
#'   inches. The defaults are 10 by 7 inches.
#'
#' @import ggplot2
#' @import reshape2
#'
#' @author Chantel Wetzel
#' @export
#'
#' @examples
#' \dontrun{
#' plot_comps(data = LFs)
#' }
#'
plot_comps <- function(
    data,
    dir = NULL,
    add_save_name = NULL,
    plot = 1:2,
    add_0_ylim = TRUE,
    width = 10,
    height = 7) {
  # if data is a list with both sexed and unsexed fish, choose sexed fish
  if ("sexed" %in% names(data) & "unsexed" %in% names(data)) {
    data <- data$sexed
    message("data input includes both sexed and unsexed comps, plotting sexed comps only")
  }
  # if data is just sexed or unsexed, pull that dataframe from the list
  if (length(data) == 1 && names(data) %in% c("sexed", "unsexed")) {
    data <- data[[1]]
  }

  # determine length vs age comps based on the presence of "age" in the column names
  data_type <- ifelse(any(grepl("age", names(data))), "age", "length")
  sex_type <- unique(data$sex)
  input_nsamp <- which(colnames(data) %in% c("nsamp", "InputN", "input_n"))
  if (is.numeric(data[, input_nsamp])) {
    N <- data[, input_nsamp]
  } else {
    N <- rep(1, nrow(data))
  }

  plotdir <- file.path(dir, "plots")
  check_dir(dir = plotdir)

  plot_names <- file.path(
    plotdir,
    paste0(
      add_save_name,
      ifelse(test = is.null(add_save_name), yes = "", no = "_"),
      c(
        paste0(data_type, "_frequency_sex_", sex_type, ".png"),
        paste0(data_type, "_r4ss_frequency_sex_", sex_type, ".png")
      )
    )
  )

  year <- as.numeric(as.character(data$year))
  sex <- unique(data$sex)
  if (length(sex) > 1) {
    cli::cli_abort("This function does not work on processed composition
      files with multiple Stock Synthesis sex specifications
      (sex = 0, sex = 1, sex = 3). Please filter file down to
      a single SS3 sex type and re-run.")
  }
  if (data_type == "length") {
    comps <- data[, -c(1:6)]
  }
  if (data_type == "age") {
    comps <- data[, -c(1:9)]
  }

  if (length(grep(".999", names(comps))) > 0) {
    # remove extra columns (if the user didn't remove them already)
    comps <- comps[, -match(".999", names(comps))]
  }

  ## column names for two-sex models now have separate "f" and "m" columns
  # # Check to see if the unsexed or single sexed comps are
  # # double printed
  # if (sum(grepl(".", colnames(comps), fixed = TRUE)) > 0) {
  #   comps <- comps[, !grepl(".", colnames(comps), fixed = TRUE)]
  # }

  # Determine if entries are proportions (e.g., sum to 1 or 100)
  # and convert if needed
  if (sum(as.numeric(comps[1, ])) > 0.999 & sum(as.numeric(comps[1, ])) < 1.001) {
    comps <- 100 * comps
  }
  if (sum(as.numeric(comps[1, ])) != 100) {
    comps <- 100 * comps / apply(comps, 1, sum)
  }

  mod_comps <- cbind(year, comps)
  df <- reshape2::melt(mod_comps, id = "year")
  df$year <- factor(df$year, levels = unique(df$year))
  df$sex <- substr(df$variable, 1, 1)
  df$sex <- replace(df$sex, df$sex %in% c("f", "F"), "FEMALE")
  df$sex <- replace(df$sex, df$sex %in% c("m", "M"), "MALE")
  df$sex <- replace(df$sex, df$sex %in% c("u", "U"), "UNSEXED")
  df$sex <- factor(df$sex, levels = unique(df$sex))
  df$variable <- utils::type.convert(gsub("[FMUfmu]", "", df$variable), as.is = TRUE)
  df$n <- 0
  a <- 1
  for (y in year) {
    df$n[df$year == y] <- N[a]
    a <- a + 1
  }

  ylabel <- ifelse(data_type == "length", "Length (cm)", "Age (yr)")
  bub_step <- ifelse(max(df$value) < 50, 5, 10)
  bub_range <- c(1, seq(bub_step, floor(max(df$value)), bub_step))
  max_range <- 15
  if (max(df$variable) - min(df$variable) >= 40) {
    y_axis <- seq(
      plyr::round_any(min(df$variable), 10, floor),
      plyr::round_any(max(df$variable), 10, ceiling),
      by = 10
    )
  } else {
    y_axis <- seq(
      plyr::round_any(min(df$variable), 5, floor),
      plyr::round_any(max(df$variable), 5, ceiling),
      by = 5
    )
  }

  igroup <- 1
  if (igroup %in% plot) {
    p <- ggplot2::ggplot(
      data = df |> dplyr::filter(value > 0),
      aes(x = year, y = variable)
    ) +
      geom_point(aes(size = value, fill = sex, colour = sex),
        alpha = 0.75, shape = 21
      ) +
      scale_fill_manual(values = c("FEMALE" = "red", "MALE" = "blue", "UNSEXED" = "darkseagreen")) +
      scale_color_manual(values = c("FEMALE" = "darkred", "MALE" = "darkblue", "UNSEXED" = "darkgreen")) +
      scale_size_continuous(
        range = c(1, 15),
        breaks = bub_range
      ) +
      facet_grid(sex ~ .) +
      scale_y_continuous(
        breaks = y_axis,
        limits = if (add_0_ylim) {
          c(0, max(y_axis))
        } else {
          c(NA, max(y_axis))
        }
      ) +
      labs(x = "Year", y = ylabel, size = "Relative\nAbundance (%)", fill = "") +
      theme(
        legend.key = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour = "black", size = 12, angle = 90, vjust = 0.3, hjust = 1),
        axis.text.y = element_text(colour = "black", size = 12),
        legend.text = element_text(size = 10, colour = "black"),
        legend.title = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.position = "right"
      ) +
      guides(size = "legend", color = "none", fill = "none")
    if (!is.null(dir)) {
      ggsave(
        filename = plot_names[1], plot = p,
        width = width, height = height, units = "in"
      )
    } else {
      print(p)
    }
  }

  igroup <- 2
  if (igroup %in% plot) {
    df2 <- df
    df2$value <- df2$value / 100
    df2[df2$sex == "MALE", "value"] <- -1 * df2[df2$sex == "MALE", "value"]
    values <- c("FEMALE" = "red", "MALE" = "blue", "UNSEXED" = "darkseagreen")[which(c("FEMALE", "MALE", "UNSEXED") %in% df2$sex)]
    p2 <- ggplot2::ggplot(df2, aes(x = variable, y = value)) +
      geom_line(aes(colour = sex), # add alpha = n inside the aes to shade by annual sample size
        lwd = 1.1
      ) +
      facet_wrap(facets = "year") +
      scale_fill_manual(values = values) +
      scale_color_manual(values = values) +
      labs(x = ylabel, y = "Proportion") +
      geom_hline(yintercept = 0) +
      theme(
        legend.key = element_blank(),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour = "black", size = 12, angle = 90, vjust = 0.3, hjust = 1),
        axis.text.y = element_text(colour = "black", size = 12),
        legend.text = element_text(size = 10, colour = "black"),
        legend.title = element_text(size = 12),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        legend.position = "right"
      )
    if (!is.null(dir)) {
      ggsave(
        filename = plot_names[2], plot = p2,
        width = width, height = height, units = "in"
      )
    } else {
      print(p2)
    }
  }
}
