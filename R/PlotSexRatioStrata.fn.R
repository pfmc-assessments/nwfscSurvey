#' Function to plot sex ratio by strata
#'
#' @template dir
#' @param dat A data frame of length-composition data returned from
#'   [pull_bio()].
#' @param type length/age which data type to use
#' @template strat.vars
#' @template strat.df
#' @param circleSize circle size
#' @param dopng Deprecated with {nwfscSurvey} 2.1 because providing a non-NULL
#'   value to `dir` can serve the same purpose as `dopng = TRUE` without the
#'   potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
#'   no longer have to specify `dopng` to save the plot as a png.
#' @param ...      Additional arguments for the plots
#'
#' @author Allan Hicks and Chantel Wetzel
#' @export
#' @seealso \code{\link{StrataFactors.fn}}

PlotSexRatioStrata.fn <- function(
  dir = NULL,
  dat,
  type = "length",
  strat.vars = c("Depth_m", "Latitude_dd"),
  strat.df = NULL,
  circleSize = 0.05,
  dopng = lifecycle::deprecated(), ...) {

  if (lifecycle::is_present(dopng)) {
    lifecycle::deprecate_warn(
      when = "2.1",
      what = "nwfscSurvey::PlotMap.fn(dopng =)"
    )
  }
  plotdir <- file.path(dir, "plots")
  check_dir(dir = plotdir)
  main_ <- ifelse(is.null(main), "", paste0(main, "_"))
  if (!is.null(dir)) {
    png(
      filename = file.path(
        plotdir,
        paste0(main_, "fraction_female.png")
      ),
      height = 7,
      width = 7,
      units = "in",
      res = 300
    )
    on.exit(dev.off(), add = TRUE)
  }

  row.names(strat.df) <- strat.df[, 1] # put in rownames to make easier to index later
  numStrata <- nrow(strat.df)
  ind <- !duplicated(dat$Trawl_id)
  datB <- dat[ind, c("Trawl_id", "Weight", strat.vars, "Longitude_dd", "Year", "Length_cm", "Age", "Sex")]

  datB <- data.frame(datB, stratum = StrataFactors.fn(datB, strat.vars, strat.df)) # create a new column for the stratum factor

  par(mfrow = c(3, 3))

  for (i in 1:length(row.names(strat.df))) {
    z <- which(datB$stratum == row.names(strat.df)[i])
    subDF <- datB[z, ]

    if (type == "length") {
      temp <- table(subDF$Length_cm, subDF$Sex)
      axis.name <- "Length (cm)"
    }
    if (type == "age") {
      temp <- table(subDF$Age, subDF$Sex)
      axis.name <- "Age"
    }

    ratioF <- temp[, "F"] / (temp[, "M"] + temp[, "F"])
    nobs <- temp[, "F"] + temp[, "M"]
    plot(ratioF, type = "l", col = "red", xlab = axis.name, ylab = "Fraction female", main = row.names(strat.df)[i], ylim = c(0, 1)) # ,...)
    symbols(ratioF, circles = nobs, inches = circleSize, fg = "red", bg = rgb(1, 0, 0, alpha = 0.5), add = T)
  }
}
