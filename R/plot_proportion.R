#' Plot proportions of factor levels (y axis) by bins (x axis)
#'
#' Bin a numerical column for the categorical x axis. Calculate the proportion
#' of the rows that are present for each level of an additional factor column,
#' like sex, by x-axis bins.
#'
#' @details [{ggmosaic}](https://haleyjeppson.github.io/ggmosaic/) is used to
#' create a {ggplot2} object for categorical data that can be modified upon
#' return.
#'
#' @param data A data frame.
#' @param column_factor <[`data-masking`][dplyr_data_masking]> Variable in
#'   `data` for the grouping structure. Should be a variable in `data` that
#'   contains a factor but it can also be a variable in `data` that stores
#'   characters. See the examples for ideas.
#' @param column_bin <[`data-masking`][dplyr_data_masking]> Variable in `data`
#'   for the binning structure. If this is not already a factor or character,
#'   then [ggplot2::cut_width()] will be used to create bins. The bins are saved
#'   in `bin` in the returned {ggplot2} object.
#' @param digits A numeric value passed to the digits argument of [round()].
#'   Positive values pertain to rounding the right side of the decimal place and
#'   negative values pertain to rounding the left side of the decimal place,
#'   i.e., to the tens or hundreds with -1 and -2. The default is to round to
#'   the nearest integer using `digits = 0`.
#' @param bar_width A string of `"n"` or `"equal"`, where the former leads to
#'   bar widths based on the number of observations contained in that group and
#'   the latter leads to equally-sized bars for all bars with data. For groups
#'   without any data, the width of the placeholder on the x axis will be
#'   smaller than the width of the bars for groups with data regardless of which
#'   option you choose. The default is to have bars of variable widths, i.e.,
#'   `"n"`.
#' @param ... Additional arguments that users want to pass to
#'   [ggplot2::cut_width()]. If `data[[column_bin]]` is not a factor, then at
#'   least `width` needs to be passed to create bins. But, any argument accepted
#'   by [ggplot2::cut_width()] can be passed, where `boundary = 0` is common so
#'   the calculation of bins will start at zero rather than the minimum value
#'   present, preventing bins like (35.4, 36.4] when you really want (35, 36].
#'
#' @author Ian G. Taylor, Chantel R. Wetzel, Kelli F. Johnson
#' @export
#' @return A {ggplot2} object created with {ggmosaic} features.
#' @seealso
#' * [ggplot2::cut_width()]
#' * [ggmosaic::geom_mosaic()]
#' * [factor()]
#' @family plot_
#' @examples
#' # Add presence/absence factor to data
#'  temp <- catch_nwfsc_combo %>%
#'    dplyr::mutate(new = factor(
#'      cpue_kg_km2 <= 0,
#'      levels = c(FALSE, TRUE),
#'      labels = c("Present", "Absent")
#'    ))
#'
#'  # Plot depth bins (50 m) by presence/absence with default colors
#'  plot_proportion(
#'    data = temp,
#'    column_factor = new,
#'    column_bin = Depth_m,
#'    width = 50,
#'    boundary = 0
#'  )
#' # Plot latitude bins (1 decimal degree) by presence/absence with custom
#' # colors
#' plot_proportion(
#'   data = temp,
#'   column_factor = new,
#'   column_bin = Latitude_dd,
#'   width = 1,
#'   boundary = 0
#' ) +
#'   ggplot2::scale_fill_manual(values = c(
#'     "darkorchid3",
#'     grDevices::gray(0.7)
#'   ))
#' # Plot depth bins (25 m) by sex (F, M, U)
#' plot_proportion(
#'   data = bio_nwfsc_combo %>%
#'     dplyr::mutate(Sex = codify_sex(Sex)),
#'   column_factor = Sex,
#'   column_bin = Depth_m,
#'   width = 25,
#'   boundary = 0
#' )
#' # Change to equal sized bars
#' plot_proportion(
#'   data = bio_nwfsc_combo %>%
#'     dplyr::mutate(Sex = codify_sex(Sex)),
#'   column_factor = Sex,
#'   column_bin = Depth_m,
#'   width = 25,
#'   boundary = 0,
#'   bar_width = "equal"
#' )
plot_proportion <- function(data,
                            column_factor,
                            column_bin,
                            digits = 0,
                            bar_width = c("n", "equal"),
                            ...) {
  # Set up
  # Create a character string of input column name
  character_bin <- as.character(ensym(column_bin))
  # match.arg allows for the default arg to be the first of the vector
  # in the function definition
  bar_width <- match.arg(bar_width)

  # Create data_plot in two steps
  # 1. If user supplies "width" as an argument, then turn the numeric column
  #    into a factor/character, else assign column to calc_bin. Must use
  #    .data[[string]] because I cannot use {{}} inside of the if statement.
  data_plot <- dplyr::mutate(
    .data = data,
    calc_bin = if ("width" %in% names(list(...))) {
      ggplot2::cut_width(round(.data[[character_bin]], digits = digits), ...)
    } else {
      .data[[character_bin]]
    }
  )
  # 2. bar_width is used to create equal-sized bars in histogram or bar widths
  #    based on the number of observations in the group. geom_mosaic will sum
  #    calc_weight by group to determine the width. Here I reverse-engineer that
  #    to ensure the summed weight will equal 1 for every group to get
  #    equal-sized bars if bar_width is "equal".
  data_plot <- data_plot %>%
    dplyr::group_by(calc_bin, .add = TRUE) %>%
    dplyr::mutate(
      calc_weight = dplyr::case_when(
        bar_width == "n" ~ 1,
        bar_width == "equal" ~ 1/n()
      )
    ) %>%
    dplyr::ungroup()

  # Create {ggplot2} figure
  # Remove scale_y_continuous if you don't want numeric values on the y axis and
  # you want the colors labeled with names instead along the axis. Could remove
  # the legend if we removed scale_y_continuous
  # vjust on x axis ensures text is centered in bin, angle 90 makes it vertical
  gg <- ggplot2::ggplot(data = data_plot) +
    ggmosaic::geom_mosaic(
      ggplot2::aes(
        x = ggmosaic::product(calc_bin), # calc proportions per bin
        fill = {{column_factor}},
        weight = calc_weight # bar width
      ),
      offset = 0.01 # provides space between the bars
    ) +
    ggplot2::xlab(pretty_label_column(character_bin)) +
    ggplot2::ylab("Proportion") +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "top",
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggokabeito::scale_fill_okabe_ito() + # color-blind friendly
    ggmosaic::scale_x_productlist()

  return(gg)
}

#' Save figures of proportions by depth and latitude using warehouse data
#'
#' Four figures in total are created and saved to the disk if both catch and
#' biological data, pulled from the data warehouse, are passed to `data_catch` and
#' `data_bio`, respectively. This function will only work with data that has the
#' standard column names of data pulled from the warehouse.
#'
#' @param data_catch,data_bio Data frames returned from [pull_catch()] and
#'   [pull_bio())], respectively. At least one of the arguments must be passed.
#' @param dir The directory where you would like the `.png` files to be saved.
#'   The default is a directory called `"plots"` in your current working
#'   directory.
#' @inheritParams plot_proportion
#' @export
#' @author Chantel R. Wetzel and Kelli F. Johnson
#' @return Strings of the saved files.
#' @seealso
#' * [plot_proportion()]
#' * [purrr::map()]
#' @family warehouse
#' @examples
#' \dontrun{
#' test <- wh_plot_proportion(catch_nwfsc_combo, bio_nwfsc_combo)
#' }
wh_plot_proportion <- function(data_catch,
                               data_bio,
                               dir = file.path(getwd(), "plots"),
                               bar_width = c("n", "equal")) {
  # Input checks
  stopifnot(any(c(!missing(data_catch), !missing(data_bio))))

  # Paste relevant prefixes to two suffixes and make them file paths
  files_all <- file.path(
    dir,
    t(outer(
      X = c(
        if(!missing(data_catch)) {"presence-absence"},
        if(!missing(data_bio)) {"sex"}
      ),
      Y = paste0("_by_", c("depth", "latitude"), ".png"),
      FUN = paste0
    ))
  )

  # Create a list of data frames
  # mutate the data frames to have the_factor that is passed to plot_proportion
  # rerun repeats the augmented data frame .n times
  data <- c(
    if (!missing(data_catch)) {
      dplyr::mutate(data_catch, the_factor = factor(
        cpue_kg_km2 <= 0,
        levels = c(FALSE, TRUE),
        labels = c("Present", "Absent")
      )) %>%
        purrr::rerun(.n = 2)
    },
    if (!missing(data_bio)) {
      dplyr::mutate(data_bio, the_factor = codify_sex(Sex)) %>%
        purrr::rerun(.n = 2)
    }
  )

  # Use map to iterate over the list of data frames and other elements of .l;
  # the nth element of each vector/list passed in .l will be passed in turn
  # to the function passed to .f; extra arguments that do not change for each
  # plot are passed after .f
  gg_all <- purrr::pmap(
    .l = list(
      pdata = data,
      x = rep(ggplot2::quos(Depth_m, Latitude_dd), length(data) / 2),
      width = rep(c(50, 1), length(data) / 2)
    ),
    .f =  function(pdata, x, width, bar_width) {
      gg <- plot_proportion(
        data = pdata,
        column_factor = the_factor,
        column_bin = !!x,
        width = width,
        bar_width = bar_width,
        boundary = 0
      )
    },
    bar_width = match.arg(bar_width)
  )

  # Save each returned ggplot object using the created file names
  files_out <- purrr::map2_chr(
    .x = files_all,
    .y = gg_all,
    .f = ggplot2::ggsave,
    height = 7,
    width = 7
  )

  # Return the file names in case users want to call them into a .Rmd file
  return(files_out)
}
