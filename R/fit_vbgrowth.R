#' Estimate von Bertanlaffy growth parameters from lengths and ages
#'
#' @details
#' Estimate von Bertanlaffy growth parameters from length and age data or
#' predicted lengths given ages and input parameters. This is the estimation
#' function that is called by the [est_growth()] function.
#'
#' @param par A list of von Bertanlaffy growth parameters in log space ordered
#' as follows: K, Linf, L0, CV0, and CV1.
#' Names will be assigned if they are not provided.
#' @param age_years A vector of ages in years. Values of \code{NA} are accepted.
#' @param length_cm A vector of lengths in cm. Lengths can be \code{NULL} if
#' \code{ReturnType == "Pred"} because you are only predicting using ages, where
#' the lengths are just needed for estimation purposes. If not \code{NULL},
#' ensure that there is one length measurement for every age measurement.
#' Values of \code{NA} are accepted.
#' @param par_logspace TRUE/FALSE Indicates if the values in the \code{"par"} are in
#' logspace and need to be converted back to normal spaces.
#' @param ReturnType A single character value with \code{"NLL"} being the default,
#' which leads to the negative log-likelihood value being returned.
#' If \code{"Pred"}, then three values are returned
#' for each combination of length and age, low, prediction, and high
#' based on the input parameters and standard deviation factor,
#' i.e., \code{sdFactor}.
#' @param sdFactor The number of standard deviations to include in the
#' low and high calculations. The default is 1.0.
#'
#' @importFrom stats dnorm
#' @export
#' @family biology function
#' @return
#' Depending on ReturnType, either the negative log likelihood is returned based
#' on fits to the data or a matrix of three columns with low, predicted, and high
#' values for each combination of length and age. Distance of the low and high
#' from the predicted value depends on the \code{sdFactor}, allowing confidence
#' intervals based on normal theory or other theories to be created.
#'
#'
fit_vbgrowth <- function(
  par,
  age_years,
  length_cm,
  par_logspace = TRUE,
  ReturnType = c("NLL", "Pred"),
  sdFactor = 1
) {
  #### Initialization
  if (is.null(names(par))) {
    names(par) <- c("K", "Linf", "L0", "CV0", "CV1")
  }
  if (!is.null(length_cm)) {
    stopifnot(
      "age_years & length_cm are not the same length" = length(age_years) ==
        length(length_cm)
    )
  }
  ReturnType <- match.arg(ReturnType, several.ok = FALSE)
  # Exponentiate parameters, which are provided in log space such that estimates are not
  # negative when using optim.
  if (par_logspace == TRUE) {
    par <- lapply(par, exp)
  }

  #### Parameters
  # Parameterized in terms of L0 instead of t0, where this parameterization was taken from
  # PacFIN.Utilities.
  # See Ogle and Isermann (2017) 10.1080/02755947.2017.1342725
  lhat.L0 <- function(par, age_years, variability = 0) {
    par[["Linf"]] -
      (par[["Linf"]] - par[["L0"]]) * exp(-age_years * par[["K"]]) +
      variability
  }
  if ("L0" %in% names(par)) {
    lhat.here <- lhat.L0
    Lhat <- lhat.here(par, age_years)
    CV <- par[["CV0"]] +
      (par[["CV1"]] - par[["CV0"]]) * (Lhat - par[["L0"]]) / par[["Linf"]]
    SD <- CV * Lhat
  }
  # todo: figure out CV of this parameterization and implement a switch if parameter t0
  # is present then this version would be used. Will also need to fix the equations
  # below

  # Parameterized in terms of t0
  lhat.t0 <- function(par, age_years, variability = 0) {
    par[["Linf"]] *
      (1 - exp(-par[["K"]] * (age_years - par[["t0"]]))) +
      variability
  }
  if ("t0" %in% names(par)) {
    cli::cli_abort(
      "t0 parameterization is not complete, please contact developers to estimate CV"
    )
    lhat.here <- lhat.t0
    Lhat <- lhat.here(par, age_years)
    CV <- par[["CV1"]]
    SD <- abs(CV * Lhat)
  }

  #### Return
  if (ReturnType == "NLL") {
    Return <- -1 *
      sum(dnorm(length_cm, mean = Lhat, sd = SD, log = TRUE), na.rm = TRUE)
  }
  if (ReturnType == "Pred") {
    Lhat_low <- lhat.here(par, age_years, variability = (-sdFactor * SD))
    Lhat_high <- lhat.here(par, age_years, variability = (sdFactor * SD))
    Return <- cbind(
      Lhat_low = Lhat_low,
      Lhat_pred = Lhat,
      Lhat_high = Lhat_high
    )
  }
  return(Return)
}
