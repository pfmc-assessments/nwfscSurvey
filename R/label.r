label_axes <- function() {
  list(
    ggplot2::xlab("Longitude"),
    ggplot2::ylab("Latitude")
  )
}

label_land <- function(size = 3) {
  # todo: find a way to limit the labels based on the
  #       extent of the figure
  info_labels <- data.frame(
    label = c("California", "Oregon", "Washington", "Canada"),
    lon = c(-119.7, -120.5, -119.8, -118.5),
    lat = c(36.778259, 43.5, 47.751076, 49.7)
  )
  list(
    ggplot2::geom_text(
      data = info_labels,
      ggplot2::aes(
        fontface = 2,
        x = lon,
        y = lat,
        label = label
      ),
      color = "black",
      # size = size,
      check_overlap = FALSE
    )
  )
}
