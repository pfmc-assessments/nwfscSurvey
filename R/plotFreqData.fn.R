#' Plot length or age compositions by year in bubble plots
#'
#' @template dir
#' @param dat Object created by [SurveyLFs.fn()] or [SurveyAFs.fn()]
#' @param inch input to the symbols plot: TRUE, FALSE or a positive number.
#' @param ylab y-axis text label
#' @param xlab x-axis text label
#' @param zero2NAs A logical specifying if `NA`s should be changed to zeros.
#'   The default is `TRUE`.
#' @param main main plot text
#' @param xlim x-limit values
#' @param ymax Value used to truncate y-axis, defaults to NULL
#' @param dopng Deprecated with {nwfscSurvey} 2.1 because providing a non-NULL
#'   value to `dir` can serve the same purpose as `dopng = TRUE` without the
#'   potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
#'   no longer have to specify `dopng` to save the plot as a png.
#' @param w Numeric figure width, defaults to 7
#' @param h Numeric figure height, defaults to 7
#' @param ...      Additional arguments for the plots
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export

PlotFreqData.fn <- function(
  dir = NULL,
  dat,
  inch = 0.15,
  ylab = "Bins",
  xlab = "Year",
  zero2NAs = TRUE,
  main = NULL,
  xlim = NULL,
  ymax = NULL,
  dopng = lifecycle::deprecated(),
  w = 7,
  h = 7,
  ...) {

  if (lifecycle::is_present(dopng)) {
    lifecycle::deprecate_warn(
      when = "2.1",
      what = "nwfscSurvey::PlotMap.fn(dopng =)"
    )
  }

  dataType <- sum(names(dat) == "ageErr")
  dataType <- ifelse(dataType == 0, "Length", "Age")

  plotdir <- file.path(dir, "plots")
  check_dir(dir = plotdir)
  main_ <- ifelse(is.null(main), "", paste0(main, "_"))
  if (!is.null(dir)) {
    png(
      filename = file.path(
        plotdir,
        paste0(main_, dataType, "_Frequency.png")
      ),
      height = h,
      width = w,
      units = "in",
      res = 300
    )
    on.exit(dev.off(), add = TRUE)
  }

  x <- as.numeric(as.character(dat$year))
  sex <- dat$sex[1]
  if (dataType == "Length") {
    dat <- dat[, -c(1:6)]
  }
  if (dataType == "Age") {
    dat <- dat[, -c(1:9)]
  }

  if (length(grep(".999", names(dat)) > 0)) {
    # remove extra columns (if the user didn't remove them already)
    if (sex == 0) {
      dat <- dat[, -match("U.999", names(dat))]
    }
    if (sex == 3) {
      # exclude columns for fish below minimum bin
      dat <- dat[, -match("F.999", names(dat))]
      dat <- dat[, -match("M.999", names(dat))]
    }
  }

  if (sex == 3) {
    div <- 2
  } else {
    div <- 1
  }
  numLens <- ncol(dat) / div

  if (!is.null(ymax)) {
    numLens <- ymax
  }

  y <- as.numeric(substring(names(dat), 2))
  y <- abs(y)
  y <- y[1:numLens]

  if (zero2NAs) {
    dat[dat == 0] <- NA
  }

  if (is.null(xlim)) {
    xlim <- range(x)
  }


  if (sex == 3) {
    par(mfrow = c(2, 1))
  }
  if (sex == 0) {
    par(mfrow = c(1, 1))
  }

  if (sex == 0) {
    if (is.null(main)) {
      main <- "Unsexed+Males+Females"
    }
    z <- c(unlist(dat[, 1:numLens]), min(dat, na.rm = TRUE)) # Changed from max to min to better visualize subset data
    symbols(c(rep(x, length(y)), 0), c(rep(y, each = length(x)), 0),
      circles = z,
      main = main, inches = inch, xlab = xlab, ylab = ylab, xlim = xlim, ...
    )
  }
  if (sex == 3) {
    name <- "Female"
    z <- c(unlist(dat[, 1:numLens]), min(dat, na.rm = TRUE))
    symbols(c(rep(x, length(y)), 0), c(rep(y, each = length(x)), 0), circles = z, main = name, inches = inch, xlab = xlab, ylab = ylab, xlim = xlim, ...)
    name <- "Male"
    z <- c(unlist(dat[, (numLens + 1):ncol(dat)]), min(dat, na.rm = TRUE))
    symbols(c(rep(x, length(y)), 0), c(rep(y, each = length(x)), 0), circles = z, main = name, inches = inch, xlab = xlab, ylab = ylab, xlim = xlim, ...)
  }
}
