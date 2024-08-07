#' Plot the represntativeness of age sampling based on lengths
#'
#' @param data data frame
#' @param xlim x limits for plot, defaults to c(0,120)
#' @param ylim y limits for plot, defaults to (0, 0.049)
#' @param dir Defaults to NULL (plot made, but not saved to file). Can alternatively be a string filename
#' by year and sex

#' @importFrom graphics grid hist
#' @export

plot_age_length_sampling <- function(data,
                                     xlim = c(0, 120),
                                     ylim = c(0, 0.049),
                                     dir = NULL) {
  if (!is.null(dir)) {
    png(
      filename = file.path(dir, "plots", "age_length_comparison.png"),
      width = 10, height = 7, units = "in", res = 300
    )
  }
  # make multi-panel plot comparing length samples to the subset with ages
  par(
    mfcol = c(5, 4),
    mar = c(0.2, 0.2, 0.2, 0.2),
    oma = c(4, 4, 1, 1)
  )
  # vector of years with age samples
  years <- sort(unique(data$Year))
  colvec <- c(rgb(1, 0, 0, alpha = 0.8), rgb(0, 0, 1, alpha = 0.5))

  # empty plot for legend
  plot(0, type = "n", axes = FALSE)
  legend("left",
    bty = "n",
    fill = colvec,
    cex = 1.5,
    legend = c(
      "All length samples",
      "Lengths of aged fish"
    )
  )

  mtext("Length (cm)", side = 1, line = 2.5, outer = TRUE)

  for (y in years) {
    # make empty plot (note: xlim and ylim were set by trial and error)
    plot(0,
      type = "n",
      xlim = xlim,
      xaxs = "i",
      ylim = ylim,
      yaxs = "i",
      axes = FALSE
    )
    grid()
    if (par()$mfg[2] == 1) {
      axis(2, las = 1)
    }
    if (par()$mfg[1] == par()$mfg[3] | y == max(years)) {
      axis(1)
    }
    lengths.y <- data$Length_cm[data$Year == y]
    ages.y <- data$Length_cm[data$Year == y &
      !is.na(data$Age)]
    hist(lengths.y,
      breaks = seq(0, 120, 5),
      freq = FALSE,
      col = colvec[1],
      add = TRUE
    )
    if (length(ages.y > 0)) {
      hist(ages.y,
        breaks = seq(0, 120, 5),
        freq = FALSE,
        col = colvec[2],
        add = TRUE
      )
    }
    legend("topleft", legend = NA, bty = "n", title = y, cex = 1.5)
    legend("right",
      legend = NA, bty = "n",
      title = paste0(
        "N lens = ",
        length(lengths.y),
        "\nN ages = ",
        length(ages.y),
        " (",
        round(100 * length(ages.y) / length(lengths.y)),
        "%)"
      ),
      cex = 1.0
    )
  }

  if (!is.null(file)) {
    dev.off()
  }
}
