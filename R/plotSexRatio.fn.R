#' Function to plot sex ratio
#'
#' @template dir
#' @param dat Data object with biological data from [pull_bio()] with a column
#' named `Sex` present.
#' @param data.type Specify where to calculate the sex ration by length or age.
#' @param main Name that will be used to name the saved png
#' @param circleSize circle size
#' @param dopng Deprecated with {nwfscSurvey} 2.1 because providing a non-NULL
#'   value to `dir` can serve the same purpose as `dopng = TRUE` without the
#'   potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
#'   no longer have to specify `dopng` to save the plot as a png.
#' @param ...      Additional arguments for the plots
#'
#' @author Allan Hicks and Chantel Wetzel
#' @importFrom graphics abline
#' @export

PlotSexRatio.fn <- function(
  dir,
  dat,
  data.type = "length",
  main = NULL,
  circleSize = 0.1,
  dopng = lifecycle::deprecated(),
  ...
) {
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
        paste0(main_, data.type, "_fraction_female.png")
      ),
      height = 7,
      width = 7,
      units = "in",
      res = 300
    )
    on.exit(dev.off(), add = TRUE)
  }

  round_any <- function(x, accuracy, f = round) {
    f(x / accuracy) * accuracy
  }

  if (data.type == "length") {
    bin_width <- 2
    dat$bin <- round_any(dat$Length_cm, bin_width, floor)
    axis.name <- "Length (cm)"
  }
  if (data.type == "age") {
    bin_width <- 1
    dat$bin <- round_any(dat$Age, bin_width, floor)
    axis.name <- "Age (yr)"
  }
  temp <- table(dat$bin, dat$Sex)

  ratioF <- temp[, "F"] / (temp[, "M"] + temp[, "F"])
  nobs <- temp[, "F"] + temp[, "M"]

  par(mfrow = c(1, 1))
  plot(
    x = names(ratioF), y = ratioF, type = "l", col = "red", lty = 2,
    xlab = axis.name, ylim = c(0, 1), main = main, ylab = "Fraction female", ...
  )
  abline(h = 0.50, col = "grey", lty = 2, lwd = 2)
  symbols(
    x = names(ratioF), y = ratioF, circles = nobs,
    inches = circleSize, fg = "red", bg = rgb(1, 0, 0, alpha = 0.5),
    add = TRUE
  )

  test <- dplyr::count(dat, bin, Sex) |>
    mutate(Proportion = n / sum(n))

  p <- ggplot(test, aes(x = bin, y = Proportion, fill = Sex)) +
    geom_bar(position = "fill", stat = "identity") +
    geom_hline(yintercept = 0.50, col = "white", lwd = 2) +
    scale_fill_manual(values = c("F" = "red", "M" = "blue", "U" = "forestgreen")) +
    labs(y = "Proportion by Sex", x = axis.name) +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      panel.background = element_blank(),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15)
    )
  print(p)

  if (!is.null(dir)) {
    ggsave(
      filename = file.path(dir, "plots", paste0("proportion_by_", data.type, "_sex.png")),
      width = 7, height = 7, units = "in"
    )
  }
}
