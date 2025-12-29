#' Plots the designed based biomass estimates with confidence intervals
#'
#' @template dir
#' @param dat object created by the [Biomasss.fn()]
#' @param CI A numerical value that specifies the confidence interval to return.
#' Values should be between 0.01 to 0.99.
#' @param scalar simply the divisor for the biomass
#' @param gap  a value that introduces a slight gap between the point estimate and the start of the line for the CI. A gap too large will invert the CI, making it look huge. You should know when this happens
#' @param ylab y-axis text label
#' @param xlab x-axis text label
#' @param ylim y-limits
#' @param add add additional line to plot
#' @param col color
#' @param main plot label
#' @param dopng Deprecated with {nwfscSurvey} 2.1 because providing a non-NULL
#'   value to `dir` can serve the same purpose as `dopng = TRUE` without the
#'   potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
#'   no longer have to specify `dopng` to save the plot as a png.
#' @param ...      Additional arguments for the plots
#'
#' @author Chantel Wetzel, Allan Hicks, and John Wallace
#' @export

PlotBio.fn <- function(
  dir = NULL,
  dat,
  CI = 0.95,
  scalar = 1e6,
  gap = 0.03,
  ylab = "Biomass ('000 mt)",
  xlab = "Year",
  main = NULL,
  ylim = NULL,
  add = FALSE,
  col = "black",
  dopng = lifecycle::deprecated(),
  ...
) {
  lifecycle::deprecate_soft(
    when = "2.4",
    what = "nwfscSurvey::PlotBio.fn()",
    details = "Please switch to get_design_based() and plot_index()."
  )

  if (lifecycle::is_present(dopng)) {
    lifecycle::deprecate_warn(
      when = "2.1",
      what = "nwfscSurvey::PlotBio.fn(dopng =)"
    )
  }

  if (length(names(dat)) != 3) {
    stop("This function only works with output from Biomass.fn()")
  }
  bio <- dat$Bio

  plotdir <- file.path(dir, "plots")
  check_dir(plotdir)
  main_ <- ifelse(is.null(main), "", paste0(main, "_"))
  if (!is.null(dir)) {
    png(
      filename = file.path(
        plotdir,
        paste0(main_, "_designed_based_index.png")
      ),
      height = 7,
      width = 7,
      units = "in",
      res = 300
    )
    on.exit(dev.off(), add = TRUE)
  }

  par(mfrow = c(1, 1))
  y <- as.numeric(as.character(bio$Value)) / scalar
  x <- as.numeric(as.character(bio$Year))
  se <- as.numeric(as.character(bio$seLogB))
  logB <- log(as.numeric(as.character(bio$Value)))
  ci <- exp(rbind(
    c(logB + qnorm(1 - (1 - CI) / 2) * se),
    c(logB - qnorm(1 - (1 - CI) / 2) * se)
  )) / scalar
  if (is.null(ylim)) {
    ylim <- c(0, 1.05 * max(ci, na.rm = TRUE))
  }
  gap <- gap * max(y)
  if (add) {
    points(x, y, col = col)
  } else {
    plot(x, y, ylab = ylab, xlab = xlab, ylim = ylim, main = main, col = col, ...)
  }
  segments(x, y + gap, x, ci[1, ], col = col)
  segments(x, y - gap, x, ci[2, ], col = col)
}
