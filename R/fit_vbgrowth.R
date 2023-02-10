#' Estimate von Bertanlaffy growth parameters from lengths and ages
#'
#' Estimate von Bertanlaffy growth parameters from length and age data or
#' predicted lengths given ages and input parameters.
#'
#' @param Par A list of von Bertanlaffy growth parameters in log space ordered
#' as follows: K, Linf, L0, CV0, and CV1.
#' Names will be assigned if they are not provided.
#' @param Ages A vector of ages in years. Values of \code{NA} are accepted.
#' @param Lengths A vector of Lengths in cm. Lengths can be \code{NULL} if
#' \code{ReturnType == "Pred"} because you are only predicting using ages, where
#' the lengths are just needed for estimation purpopses. If not \code{NULL},
#' ensure that there is one length measurement for every age measurement.
#' Values of \code{NA} are accepted.
#' @param par_logspace TRUE/FALSE Indicates if the values in the \code{"Par"} are in 
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
#' @return
#' Depending on ReturnType, either the negative log likelihood is returned based
#' on fits to the data or a matrix of three columns with low, predicted, and high
#' values for each combination of length and age. Distance of the low and high
#' from the predicted value depends on the \code{sdFactor}, allowing confidence
#' intervals based on normal theory or other theories to be created.
#'
#' @examples
#' \dontrun{
#' bio_dat <- data.frame(Age = rep(0:30, each = 20),
#'   Length_cm = rnorm(n = 31 * 20, mean = 50, sd = 5))
#' pars_in <- lapply(FUN = log, X = list(
#'   "K" = 0.13,
#'   "Linf" = 55,
#'   "L0" = 5,
#'   "CV0" = 0.1,
#'   "CV1" = 0.1))
#' solve <- optim(fn = estgrowth.vb, par = unlist(pars_in), hessian = FALSE,
#'   Ages = bio_dat[, "Age"],
#'   Lengths = bio_dat[, "Length_cm"])
#' predictions <- estgrowth.vb(Par = solve$par, ReturnType = "Pred",
#'   sdFactor = 1,
#'   Ages = bio_dat[, "Age"],
#'   Lengths = bio_dat[, "Length_cm"])
#' plot(bio_dat$Age, predictions[, "Lhat_pred"],
#'   xlab = "Age (years)", ylab = "Predicted length (cm)")
#' exp(solve$par)
#' }

fit_vbgrowth <- function(Par, Ages, Lengths, par_logspace = TRUE, ReturnType = c("NLL", "Pred"), sdFactor = 1) {

  #### Initialization
  if (is.null(names(Par))) {
    names(Par) <- c("K", "Linf", "L0", "CV0", "CV1")
  }
  if (!is.null(Lengths)) {
    stopifnot("Ages & Lengths are not the same length" = length(Ages) == length(Lengths))
  }
  ReturnType <- match.arg(ReturnType, several.ok = FALSE)
  # Exponentiate parameters, which are provided in log space such that estiamtes are not
  # negative when using optim.
  if(par_logspace == TRUE){
    Par <- lapply(Par, exp)
  } 

  #### Parameters
  # Parameterized in terms of L0 instead of t0, where this parameterization was taken from
  # PacFIN.Utilities.
  # See Ogle and Isermann (2017) 10.1080/02755947.2017.1342725
  lhat.L0 <- function(Par, Ages, variability = 0) {
    Par[["Linf"]] - (Par[["Linf"]] - Par[["L0"]]) * exp(-Ages * Par[["K"]]) + variability
  }
  if ("L0" %in% names(Par)) {
    lhat.here <- lhat.L0
    Lhat <- lhat.here(Par, Ages)
    CV <- Par[["CV0"]] + (Par[["CV1"]] - Par[["CV0"]]) * (Lhat - Par[["L0"]]) / Par[["Linf"]]
    SD <- CV * Lhat
  }
  # todo: figure out CV of this parameterization and implement a switch if parameter t0
  # is present then this version would be used. Will also need to fix the equations
  # below

  # Parameterized in terms of t0
  lhat.t0 <- function(Par, Ages, variability = 0) {
    Par[["Linf"]] * (1 - exp(-Par[["K"]] * (Ages - Par[["t0"]]))) + variability
  }
  if ("t0" %in% names(Par)) {
    stop("t0 parameterization is not complete, please contact developers to estimate CV")
    lhat.here <- lhat.t0
    Lhat <- lhat.here(Par, Ages)
    CV <- Par[["CV1"]]
    SD <- abs(CV * Lhat)
  }

  #### Return
  if (ReturnType == "NLL") {
    Return <- -1 * sum(dnorm(Lengths, mean = Lhat, sd = SD, log = TRUE), na.rm = TRUE)
  }
  if (ReturnType == "Pred") {
    Lhat_low   <- lhat.here(Par, Ages, variability = (-sdFactor * SD))
    Lhat_high  <- lhat.here(Par, Ages, variability = (sdFactor * SD))
    Return <- cbind(Lhat_low = Lhat_low, Lhat_pred = Lhat, Lhat_high = Lhat_high)
  }
  return(Return)
}
