#' Plots the biomass with confidence intervals
#'
#' @param dir directory to save the file
#' @param dat object created by the GetTotalBiomass.fn
#' @param CI confidence interval
#' @param scalar simply the divisor for the biomass
#' @param gap  a value that introduces a slight gap between the point estimate and the start of the line for the CI. A gap too large will invert the CI, making it look huge. You should know when this happens
#' @param ylab y-axis text label
#' @param xlab x-axis text label
#' @param ylim y-limits
#' @param sameylim Boolean, whether to include same y-limits or not. Defaults to FALSE
#' @param add add additional line to plot
#' @param mfrow.in option to specify the mfrow for plotting
#' @param col color
#' @param survey.name if specified will name the output png file using this name
#' @param strata.names custom strata names, if not specified will use the defined names from CreateStrataDF.fn
#' @param pch.col Color as string, defaults to "black"
#' @param pch.type Numeric pch type, defaults to 16
#' @param dopng Deprecated with {nwfscSurvey} 2.1 because providing a non-NULL
#'   value to `dir` can serve the same purpose as `dopng = TRUE` without the
#'   potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
#'   no longer have to specify `dopng` to save the plot as a png.
#' @param ...      Additional arguments for the plots
#'
#' @author Allan Hicks and John Wallace
#' @export

PlotBioStrata.fn <- function(dir = NULL, dat, CI = 0.95, scalar = 1e6, gap = 0.03, ylab = "Biomass ('000 mt)", xlab = "Year",
                             survey.name = NULL, strata.names = NULL, ylim = NULL, sameylim = FALSE, add = FALSE, mfrow.in = NULL, col = "black",
                             pch.col = "black", pch.type = 16, dopng = lifecycle::deprecated(), ...) {

  if (lifecycle::is_present(dopng)) {
    lifecycle::deprecate_warn(
      when = "2.1",
      what = "nwfscSurvey::PlotMap.fn(dopng =)"
    )
  }

  bio_strat <- dat$StrataEsts

  plotdir <- file.path(dir, "plots")
  check_dir(dir = plotdir)
  main_ <- ifelse(is.null(survey.name), "", paste0(survey.name, "_"))
  if (!is.null(dir)) {
    png(
      filename = file.path(
        plotdir,
        paste0(main_, "designed_based_by_strata_index.png")
      ),
      height = 7,
      width = 7,
      units = "in",
      res = 300
    )
    on.exit(dev.off(), add = TRUE)
  }

  if (is.null(strata.names)) {
    strata.names <- names(bio_strat)
  }

  if (!is.null(mfrow.in)) {
    par(mfrow = c(mfrow.in[1], mfrow.in[length(mfrow.in)]))
  } else {
    par(mfrow = c(length(bio_strat) / 2, 2))
  }

  for (a in 1:length(bio_strat)) {
    df <- bio_strat[[a]]
    se <- logB <- ci <- NULL
    y <- as.numeric(as.character(df$Bhat)) / scalar
    x <- as.numeric(as.character(df$name))
    se <- as.numeric(as.character(log(df$cv^2 + 1)))
    logB <- log(as.numeric(as.character(df$Bhat)) + 0.000001)
    ci <- exp(rbind(
      c(logB + qnorm(1 - (1 - CI) / 2) * se),
      c(logB - qnorm(1 - (1 - CI) / 2) * se)
    )) / scalar

    if (sameylim & is.null(ylim)) {
      tmp <- do.call("rbind", bio_strat)
      tmpse <- as.numeric(as.character(log(tmp$cv^2 + 1)))
      tmplogB <- log(as.numeric(as.character(tmp$Bhat)) + 0.000001)
      tmpci <- exp(rbind(c(tmplogB + qnorm(1 - (1 - CI) / 2) * tmpse), c(tmplogB - qnorm(1 - (1 - CI) / 2) * tmpse))) / scalar
      ylim <- c(0, 1.05 * max(tmpci, na.rm = TRUE))
    }
    if (!sameylim) {
      ylim <- c(0, 1.05 * max(ci, na.rm = TRUE))
    }

    if (is.null(ylim)) {
      ylim <- c(0, 1.05 * max(ci, na.rm = TRUE))
    }
    # gap <- gap * max(y)
    if (add) {
      points(x, y, col = pch.col, pch = pch.type)
    }
    else {
      plot(x, y, ylab = ylab, xlab = xlab, ylim = ylim, main = strata.names[a], col = pch.col, pch = pch.type, ...)
    }
    segments(x0 = x, y0 = y + gap, x1 = x, y1 = ci[1, ], col = col)
    segments(x0 = x, y0 = y - gap, x1 = x, y1 = ci[2, ], col = col)
  }

}
