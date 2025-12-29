# Estimate von Bertanlaffy growth parameters from lengths and ages

Estimate von Bertanlaffy growth parameters from lengths and ages

## Usage

``` r
fit_vbgrowth(
  Par,
  Ages,
  Lengths,
  par_logspace = TRUE,
  ReturnType = c("NLL", "Pred"),
  sdFactor = 1
)
```

## Arguments

- Par:

  A list of von Bertanlaffy growth parameters in log space ordered as
  follows: K, Linf, L0, CV0, and CV1. Names will be assigned if they are
  not provided.

- Ages:

  A vector of ages in years. Values of `NA` are accepted.

- Lengths:

  A vector of Lengths in cm. Lengths can be `NULL` if
  `ReturnType == "Pred"` because you are only predicting using ages,
  where the lengths are just needed for estimation purposes. If not
  `NULL`, ensure that there is one length measurement for every age
  measurement. Values of `NA` are accepted.

- par_logspace:

  TRUE/FALSE Indicates if the values in the `"Par"` are in logspace and
  need to be converted back to normal spaces.

- ReturnType:

  A single character value with `"NLL"` being the default, which leads
  to the negative log-likelihood value being returned. If `"Pred"`, then
  three values are returned for each combination of length and age, low,
  prediction, and high based on the input parameters and standard
  deviation factor, i.e., `sdFactor`.

- sdFactor:

  The number of standard deviations to include in the low and high
  calculations. The default is 1.0.

## Value

Depending on ReturnType, either the negative log likelihood is returned
based on fits to the data or a matrix of three columns with low,
predicted, and high values for each combination of length and age.
Distance of the low and high from the predicted value depends on the
`sdFactor`, allowing confidence intervals based on normal theory or
other theories to be created.

## Details

Estimate von Bertanlaffy growth parameters from length and age data or
predicted lengths given ages and input parameters.

## Examples

``` r
if (FALSE) { # \dontrun{
bio_dat <- data.frame(
  Age = rep(0:30, each = 20),
  Length_cm = rnorm(n = 31 * 20, mean = 50, sd = 5)
)
pars_in <- lapply(FUN = log, X = list(
  "K" = 0.13,
  "Linf" = 55,
  "L0" = 5,
  "CV0" = 0.1,
  "CV1" = 0.1
))
solve <- optim(
  fn = estgrowth.vb, par = unlist(pars_in), hessian = FALSE,
  Ages = bio_dat[, "Age"],
  Lengths = bio_dat[, "Length_cm"]
)
predictions <- estgrowth.vb(
  Par = solve$par, ReturnType = "Pred",
  sdFactor = 1,
  Ages = bio_dat[, "Age"],
  Lengths = bio_dat[, "Length_cm"]
)
plot(bio_dat$Age, predictions[, "Lhat_pred"],
  xlab = "Age (years)", ylab = "Predicted length (cm)"
)
exp(solve$par)
} # }
```
