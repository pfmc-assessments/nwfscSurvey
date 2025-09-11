#' Plot variability of length at age
#'
#' @details
#' Plots the standard deviation and coefficient of variation of age at observed
#' and predicted length
#'
#'
#' @template dir
#' @param dat The data loaded from [pull_bio()]
#' @param main Name that will be used to name the saved png
#' @param bySex Logical to indicate if plot by sex
#' @param Par Starting parameters for K, Linf, L0, CV0, and CV2 based on the
#'   Stock Synthesis parameterization of von Bertanlaffy growth.
#' @param estVB Logical. Estimate vonB growth to plot against predicted length.
#'   If F, it uses the parameters in \code{parStart}.
#' @param bins The bins to put ages into. If NULL then simply uses the ages as
#'   recorded.
#' @param legX legend location for x axis, defaults to "bottomleft"
#' @param legY legend location for y axis, defaults to NULL
#' @param dopng Deprecated with {nwfscSurvey} 2.1 because providing a non-NULL
#'   value to `dir` can serve the same purpose as `dopng = TRUE` without the
#'   potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
#'   no longer have to specify `dopng` to save the plot as a png.
#' @param ... Additional arguments for the plots.
#'
#' @author Chantel Wetzel
#' @export

plot_varlenage <- function(
    dir = NULL,
    dat,
    main = NULL,
    Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10),
    bySex = TRUE,
    estVB = TRUE,
    bins = NULL,
    legX = "bottomleft",
    legY = NULL,
    dopng = lifecycle::deprecated(),
    ...) {
  if (lifecycle::is_present(dopng)) {
    lifecycle::deprecate_warn(
      when = "2.1",
      what = "nwfscSurvey::PlotMap.fn(dopng =)"
    )
  }

  calc_vb <- function(age, k, Linf, L0) {
    out <- Linf - (Linf - L0) * exp(-age * k)
    return(out)
  }

  plotdir <- file.path(dir, "plots")
  check_dir(dir = plotdir)
  main_ <- ifelse(is.null(main), "", paste0(main, "_"))

  if (!is.null(dir)) {
    png(
      filename = file.path(
        plotdir,
        paste0(main_, "VarLengthAtAge_.png")
      ),
      height = 7,
      width = 7,
      units = "in",
      res = 300
    )
    on.exit(dev.off(), add = TRUE)
  }

  par(mfcol = c(2, nn), mar = c(4, 4, 4, 4), oma = c(2, 2, 2, 2))

  ests <- est_growth(
    dir = dir, dat = dat, Par = Par, return_df = FALSE,
    bySex = bySex, estVB = estVB, bins = bins
  )
  if (length(ests) > 2) {
    sex_names <- c("female", "male")
  } else {
    sex_names <- "unsexed"
  }
  num <- grep("sd_cv", names(ests))

  colors <- c("#440154FF", "#1F998AFF", "#CBE11EFF")
  # These colors correspond to viridis::viridis(14)[c(1, 8)]

  max_loop <- ifelse(length(num) > 1, 2, 1)

  # Loop by sex
  for (i in 1:max_loop) {
    xpar <- c(as.numeric(ests[[i]][1]), as.numeric(ests[[i]][2]), as.numeric(ests[[i]][3]))
    xsd <- ests[[num[i]]][, 2]
    xcv <- ests[[num[i]]][, 3]
    if (is.null(bins)) {
      ages <- as.numeric(rownames(ests[[num[i]]]))
    } else {
      ages <- bin[as.numeric(rownames(ests[[num[i]]]))]
    }

    plot(ages, xsd, col = colors[1], cex = 1.1, xlab = "Age", ylab = "SD of L@A", type = "b", pch = 16, lty = 1, main = names(la_data_list)[i], ...)
    par(new = T)
    plot(ages, xcv, col = colors[2], cex = 1.1, xlab = "", ylab = "", yaxt = "n", type = "b", pch = 3, lty = 2, ...)
    axis(4)
    mtext("CV", side = 4, line = 2.6)
    legend(bty = "n", x = legX, y = legY, c("SD", "CV"), col = colors, pch = c(16, 3), lty = c(1, 2))
    plot(calc_vb(age = ages, k = xpar[1], Linf = xpar[2], L0 = xpar[3]), xsd,
      xlab = "Predicted Length at Age", ylab = "SD of L@A",
      col = colors[1], cex = 1.1, type = "b", pch = 16, lty = 1, main = sex_names[i], ...
    )
    par(new = T)
    plot(calc_vb(age = ages, k = xpar[1], Linf = xpar[2], L0 = xpar[3]), xcv,
      xlab = "",
      col = colors[2], cex = 1.1, ylab = "", yaxt = "n", type = "b", pch = 3, lty = 2, ...
    )
    axis(4)
    mtext("CV", side = 4, line = 2.6)
    legend(bty = "n", col = colors, x = legX, y = legY, c("SD", "CV"), pch = c(16, 3), lty = c(1, 2))
  } # end sex loop

  if (!is.null(dir)) {
    dev.off()
    save(ests, file = file.path(dir, "growth_variance_vonB_estimates.Rdata"))
  }
}
