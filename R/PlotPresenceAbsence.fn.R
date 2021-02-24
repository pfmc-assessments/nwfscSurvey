#' Plot showing presence and absence per haul by depth or latitute bin
#'
#' Uses the data.frame of catch values extracted from the data warehouse
#' by [PullCatch.fn()] to make a table of zero and non-zero catch by
#' depth or latitute bins.
#'
#' @param catch data.frame containing catch per haul created by
#' PullCatch.fn()
#' @param dim Dimension of interest, either "depth" or "lat"
#' @param dir The directory name to print to
#' @param dopng Boolean, whether or not to do png. Defaults to FALSE,
#' @param depth_min Minimum depth (in meters)
#' @param depth_max Maximum depth (in meters). A NULL value will cause the
#' function to automatically set depth_max to the multiple of depth_bin_width beyond
#' the 99.9 percentile of the observations.
#' @param depth_bin_width Width of each depth bin (in meters)
#' @param lat_min Minimum latitute (in decimal degrees)
#' @param lat_max Maximum latitute (in decimal degrees)
#' @param lat_bin_width Width of each latitude bin (in decimal degrees)
#' @param add_range_to_main Add the range of latitude or depth by which the data
#' are filtered
#' @param main String title, defaults to "Presence/absence in WCGBT Survey"
#' @param xlab Label for x-axis. A NULL value will cause the function
#' to choose either "Depth (m)" or "Latitude (°N)"
#'
#' @author Ian G. Taylor
#' @importFrom grDevices gray
#' @export
#' @examples
#' # load WCGBTS catch data
#' catch.WCGBTS.ling <- nwfscSurvey::PullCatch.fn(
#'   Name = "lingcod",
#'   SurveyName = "NWFSC.Combo"
#' )
#' PlotPresenceAbsence.fn(catch.WCGBTS.ling, dim = "lat")
#' PlotPresenceAbsence.fn(catch.WCGBTS.ling, dim = "depth")
PlotPresenceAbsence.fn <- function(catch,
                                   dim = "depth",
                                   dir = NULL,
                                   dopng = FALSE,
                                   depth_min = 50,
                                   depth_max = NULL,
                                   depth_bin_width = 25,
                                   lat_min = 32,
                                   lat_max = 49,
                                   lat_bin_width = 1.0,
                                   main = "Presence/absence in WCGBT Survey",
                                   add_range_to_main = TRUE,
                                   xlab = NULL) {
  # check inputs
  if (!dim %in% c("depth", "lat")) {
  }

  # set depth_max if not provided
  if (is.null(depth_max)) {
    depth_999 <- quantile(
      prob = 0.999,
      x = catch$Depth_m[catch$Latitude_dd > lat_min &
        catch$Latitude_dd < lat_max &
        catch$cpue_kg_km2 > 0]
    )
    depth_max <- 2 * depth_bin_width + depth_bin_width * floor(depth_999 / depth_bin_width)
    message(
      "99.9% of positive hauls are shallower than ", round(depth_999), ".\n",
      "'depth_max' set to ", depth_max, ". Input alternative value if you wish."
    )
  }

  # filter catch based on input depth and latitute range
  catch2 <- catch[catch$Depth_m < depth_max &
    catch$Depth_m > depth_min &
    catch$Latitude_dd > lat_min &
    catch$Latitude_dd < lat_max, ]

  # note: \u00B0 is degree symbol and \u2013 is n-dash

  # depth as dimension of interest
  if (dim == "depth") {
    # get binned value
    binned_value <- depth_bin_width * floor(catch2$Depth_m / depth_bin_width)
    # update xlab and caption
    if (is.null(xlab)) {
      xlab <- "Depth (m)"
    }
    if (add_range_to_main) {
      main <- paste0(main, " (", lat_min, "\u00B0N \u2013 ", lat_max, "\u00B0N)")
    }
    filename <- "Presence-absence_by_depth_in_WCGBT_Survey.png"
  }
  # rounded value for latitude
  if (dim == "lat") {
    # get binned value
    binned_value <- lat_bin_width * floor(catch2$Latitude_dd / lat_bin_width)
    # update xlab and caption
    if (is.null(xlab)) {
      xlab <- "Latitude (\u00B0N)"
    }
    if (add_range_to_main) {
      main <- paste0(main, " (", depth_min, " \u2013 ", depth_max, "m)")
    }
    filename <- "Presence-absence_by_latitude_in_WCGBT_Survey.png"
  }

  # code in "if (dopng)" section below copied from plotBio.fn()
  if (dopng) {
    if (is.null(dir)) {
      stop("Directory needs to be set.")
    }
    if (!file.exists(dir)) {
      stop("The dir argument leads to a location", ",\ni.e., ", dir, ", that doesn't exist.")
    }
    plotdir <- file.path(dir, paste("plots", sep = ""))
    plotdir.isdir <- file.info(plotdir)$isdir
    if (is.na(plotdir.isdir) | !plotdir.isdir) {
      dir.create(plotdir)
    }
    png(file.path(dir, "plots", filename),
      height = 7, width = 7, units = "in", res = 300
    )
  }

  # make table
  tab <- table(binned_value, catch2$cpue_kg_km2 > 0)
  # add names for the dimensions which get used in the plot
  dimnames(tab)[[2]] <- c("Absent", "Present")
  # make plot
  plot(tab, col = c(gray(.7), "blue3"), cex = 1, main = main, xlab = xlab)

  # close PNG if it was used
  if (dopng) {
    dev.off()
  }
}
