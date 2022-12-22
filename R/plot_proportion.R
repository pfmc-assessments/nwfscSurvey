#' Plot showing presence and absence per haul by depth, latitude, or sex bin
#'
#' Uses the `data.frame` of data values extracted from the data warehouse
#' by either [PullCatch.fn()] or [PullBio.fn()] to make a table of
#' proportion by depth or latitude bins. If biological data are passed
#' to the function then there is a single option to plot the sex
#' ratio by depth.
#'
#' @param data `data.frame` containing data per haul created by
#' [PullCatch.fn()] or biological data created by [PullBio.fn()] where
#' `dim` must be `dim = "sex"`.
#' @param dim Dimension of interest, either "depth", "lat", or "sex".
#' @param dir The directory name to save figures to where they will be save
#' via `file.path(dir, "plots")`.
#' @param plot_type Two options area available, `"proportion"` or `"total"` where
#' the default, `"proportion"`, plots the proportion by depth/latitude with equal
#' bar widths and the `"total"` option plots the numbers by depth/latitude with
#' the bar widths in relation to sampling by depth/latitude.
#' @param depth_min Minimum depth (in meters).
#' @param depth_max Maximum depth (in meters). A NULL value will cause the
#' function to automatically set depth_max to the multiple of depth_bin_width beyond
#' the 99.9 percentile of the observations.
#' @param depth_bin_width Width of each depth bin (in meters).
#' @param lat_min Minimum latitude (in decimal degrees).
#' @param lat_max Maximum latitude (in decimal degrees).
#' @param lat_bin_width Width of each latitude bin (in decimal degrees).
#' @param add_range_to_main Add the range of latitude or depth by which the data
#' are filtered.
#' @param xlab Label for x-axis. A NULL value will cause the function
#' to choose either "Depth (m)" or "Latitude (°N)".
#'
#' @author Ian G. Taylor and Chantel Wetzel
#' @importFrom grDevices gray
#' @export
#' @examples
#' \dontrun{
#' # load WCGBTS data data
#' data.WCGBTS.ling <- nwfscSurvey::PullCatch.fn(
#'   Name = "lingcod",
#'   SurveyName = "NWFSC.Combo"
#' )
#' bio.WCGBTS.ling <- nwfscSurvey::PullBio.fn(
#'   Name = "lingcod",
#'   SurveyName = "NWFSC.Combo"
#' )
#' plot_proportion(data = data.WCGBTS.ling, dim = "lat")
#' plot_proportion(data = data.WCGBTS.ling, dim = "depth")
#' plot_proportion(data = bio.WCGBTS.ling, dim = "sex")
#' }
plot_proportion <- function(data,
  dim = c("depth", "lat", "sex"),
  dir = NULL,
  plot_type = c("proportion", "total"),
  depth_min = 50,
  depth_max = NULL,
  depth_bin_width = 25,
  lat_min = 32,
  lat_max = 49,
  lat_bin_width = 1.0,
  add_range_to_main = TRUE,
  xlab = NULL) {

  # check inputs
  dim <- match.arg(dim)
  plot_type <- match.arg(plot_type)

  if (dim == "sex"){
    if (sum(colnames(data) == "Sex") != 1) {
      stop("The data function input needs to be a biological data file.")
    }
  }

  round_any <- function(x, accuracy, f = round) {
    f(x / accuracy) * accuracy
  }

  # set depth_max if not provided
  if (is.null(depth_max)) {
    depth_999 <- stats::quantile(
      prob = 0.999,
      x = data$Depth_m[data$Latitude_dd > lat_min &
        data$Latitude_dd < lat_max &
        data$cpue_kg_km2 > 0]
    )
    depth_max <- 2 * depth_bin_width + depth_bin_width * floor(depth_999 / depth_bin_width)
    message(
      "99.9% of positive hauls are shallower than ", round(depth_999), ".\n",
      "'depth_max' set to ", depth_max, ". Input alternative value if you wish."
    )
  }

  # filter data based on input depth and latitude range
  data2 <- data[data$Depth_m < depth_max &
    data$Depth_m > depth_min &
    data$Latitude_dd > lat_min &
    data$Latitude_dd < lat_max, ]

  # note: \u00B0 is degree symbol and \u2013 is n-dash

  # depth as dimension of interest
  if (dim == "depth") {
    # get binned value
    data2$binned_value <- round_any(data2$Depth_m, depth_bin_width, floor)
    # update xlab and caption
    if (is.null(xlab)) {
      xlab <- "Depth (m)"
    }
    main = "Presence/Absence"
    if (add_range_to_main) {
      main <- paste0(main, " (", lat_min, "\u00B0N \u2013 ", lat_max, "\u00B0N)")
    }
    filename <- paste0("presence-absence_", plot_type, "_by_depth.png")
    
  }

  # rounded value for latitude
  if (dim == "lat") {
    # get binned value
    data2$binned_value <- round_any(data2$Latitude_dd, lat_bin_width, floor)
    # update xlab and caption
    if (is.null(xlab)) {
      xlab <- "Latitude (\u00B0N)"
    }
    main = "Presence/Absence"
    if (add_range_to_main) {
      main <- paste0(main, " (", depth_min, " \u2013 ", depth_max, "m)")
    }
    filename <- paste0("presence-absence_", plot_type, "_by_latitude.png")

  }

  # depth as dimension of interest
  if (dim == "sex") {
    # get binned value
    data2$binned_value <- round_any(data2$Depth_m, depth_bin_width, floor)
    # update xlab and caption
    if (is.null(xlab)) {
      xlab <- "Depth (m)"
    }
    main = "Sex Ratio by Depth"
    if (add_range_to_main) {
      main <- paste0(main, " (", lat_min, "\u00B0N \u2013 ", lat_max, "\u00B0N)")
    }

    filename <- paste0("sex_", plot_type, "_by_depth.png")
  }

  if (!is.null(dir)) {
    plotdir <- file.path(dir, paste("plots", sep = ""))
    check_dir(plotdir)
    png(file.path(dir, "plots", filename),
      height = 7, width = 7, units = "in", res = 300
    )
    on.exit(dev.off())
  }
  
  # make table
  if (dim %in% c("depth", "lat")) {
    tab <- table(data2$binned_value, data2$cpue_kg_km2 > 0)
    dimnames(tab)[[2]] <- c("Absent", "Present")

    # Calculate in terms of proportions
    prop_tab <- table(data2$binned_value, data2$cpue_kg_km2 > 0)
    prop_tab[,2] = tab[,2] / (tab[,1] + tab[,2])
    prop_tab[,1] = 1- prop_tab[,2]
    dimnames(prop_tab)[[2]] <- c("Absent", "Present")
    color <- c(gray(.7), 'darkorchid3')
  }

  if (dim == "sex") {
    tab <- table(data2$binned_value, data2$Sex == "F")
    dimnames(tab)[[2]] <- c("Male", "Female")

    # Calculate in terms of proportions
    prop_tab <- table(data2$binned_value, data2$Sex == "F")
    prop_tab[,1] = prop_tab[,1] / (prop_tab[,1] + prop_tab[,2])
    prop_tab[,2] = 1- prop_tab[,1]
    dimnames(prop_tab)[[2]] <- c("Male", "Female")
    color <- c('blue', 'red')
  }

  las_dir <- ifelse(length(unique(data2$binned_value)) > 9, 3, 1)

  # make plot
  if (plot_type == "proportion") {
    plot(prop_tab, col = color, cex = 1, main = main, 
      xlab = xlab, las = las_dir)
  }
  if (plot_type == "total") {
    plot(tab, col = color, cex = 1, main = main, 
      xlab = xlab, las = las_dir)
  }
  abline(h = 0.5, col = 'white', lty = 1, lwd = 4)

}
