#' @param bins A vector of two or more numerical values passed to
#'   `cut(breaks = bins, right = FALSE, include.lowest = TRUE)`
#'   to determine the intervals that `var` binned by.
#'   The first value will be the left limit of the first bin,
#'   which will be a closed interval to include values equal to the input.
#'   The last value will be the right limit of the last bin,
#'   which will be a closed interval to include values equal to the input.
#'   Many people like to add `-999` and `Inf` to the beginning and end of
#'   the input vector to ensure that all positive values will be assigned
#'   a bin rather than `NA`.
#'   See the details section or `?cut` for more information.
