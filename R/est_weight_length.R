#' Calculate weight-length relationship parameters
#'
#' Estimate parameters of the weight-length relationship for each
#' sex and all sexes combined, where the latter includes unsexed fish.
#'
#' @param data A data frame containing empirical weights and lengths
#' from sampled fish.
#' Sexes should be available in the column `sex` or `SEX`.
#' @param col_length A numeric or character value specifying the column
#' to use in `data` for length information. These lengths are assumed to
#' be in centimeters. The default value is `lengthcm`, which is added
#' to a data set automatically when running [cleanPacFIN].
#' @param col_weight A numeric or character value specifying the column
#' to use in `data` for weight information. These weights are assumed to
#' be in kilograms The default value is `weightkg`, which is added
#' to a data set automatically when running [cleanPacFIN].
#' Using kilograms is the default because Stock Synthesis assumes the
#' weight-length parameters are calculated using centimeters and kilograms.
#' The reported values are easily scaled to give you results in grams if
#' you wish to have more standard parameter estimates.
#' @template verbose
#'
#' @author Kelli F. Johnson and Chantel Wetzel
#' @export
#' @return A data frame of weight-length parameters by sex.
#' Parameters A and B are in the appropriate units to input
#' into Stock Synthesis Wtlen_1_Fem and Wtlen_2_Fem, or
#' Wtlen_1_Mal and Wtlen_1_Mal, parameters in the
#' control file. Values of `NA` are returned for models that
#' did not have enough data to properly estimate the parameters.
#' This will happen when there are no females in your data set,
#' for example.
#'
estimate_weight_length <- function(
  data,
  col_length = "lengthcm",
  col_weight = "weightkg",
  verbose = FALSE
  ) {

  col_length <- tolower(col_length)
  col_weight <- tolower(col_weight)
  colnames(data) <- tolower(colnames(data))
  colnames(data) <- gsub(col_weight, "weight", colnames(data))
  colnames(data) <- gsub(col_length, "length_cm", colnames(data))
  col_length <- "length_cm"
  col_weight <- "weight"
  stopifnotcolumn(data = data, string = col_length)
  stopifnotcolumn(data = data, string = col_weight)
  stopifnotcolumn(data = data, string = "sex")

  dims <- dim(data)
  data <- data[
    !is.na(data[[col_weight]]) &
    !is.na(data[[col_length]]), ]

  if (verbose) {
    message("Calculating the weight-length relationship from ",
      nrow(data), "\nfish because ", dims[1] - nrow(data),
      " fish did not have empirical weights and lengths.")
  }

  # Create a tibble data frame equal to the number of sexes
  # in the data with 3 columns
  mresults <- tibble::lst(
    female = . %>% dplyr::filter(sex == "F"),
    male = . %>% dplyr::filter(sex == "M"),
    all = . %>% dplyr::filter(sex %in% c(NA, "F", "M", "U", "H"))
    ) %>%
    purrr::map_dfr(~ tidyr::nest(.x(data), data = everything()), 
      .id = "group") %>%
    dplyr::mutate(
      fits = purrr::map(data, ~ stats::lm(log(weight) ~ log(length_cm), 
        data = .x)))

  wghtlen_ests <- mresults %>% 
    dplyr::reframe(
      group = group,
      median_intercept = purrr::map_dbl(fits, ~ exp(.x$coefficients[1])),
      SD = purrr::map_dbl(fits, ~ sd(.x$residuals)),
      A = purrr::map_dbl(fits, ~ exp(.x$coefficients[1])*exp(0.5*sd(.x$residuals)^2)),
      B = purrr::map_dbl(fits, ~ .x$coefficients[2])
      ) %>% 
    data.frame

  if (verbose) {
    message("Estimated weight-length by sex:")
    utils::capture.output(lapply(mresults[["fits"]], summary), type = "message")
  }

  return(wghtlen_ests)
}
