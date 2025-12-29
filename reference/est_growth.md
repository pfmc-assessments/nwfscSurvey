# Estimate length-at-age using the von Bertanlaffy parameterization

Estimate length-at-age using the von Bertanlaffy parameterization

## Usage

``` r
est_growth(
  dir = NULL,
  dat,
  return_df = TRUE,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.1, CV1 = 0.1),
  bySex = TRUE,
  estVB = TRUE,
  bins = NULL,
  sdFactor = 1,
  dopng = lifecycle::deprecated()
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- dat:

  Data frame of biological data from [`pull_bio()`](pull_bio.md)

- return_df:

  TRUE/FALSE If set to TRUE the dat data frame is returned with added
  columns for the estimated Lhat_low, Lhat_pred, and Lhat_high.

- Par:

  Data frame of starting parameters for K, Linf, L0, CV0, and CV2 based
  on the Stock Synthesis parameterization of von Bertanlaffy growth.

- bySex:

  Logical to indicate if plot by sex

- estVB:

  Logical. Estimate vonB growth to plot against predicted length. If F,
  it uses the parameters in `parStart`.

- bins:

  The bins to put ages into. If NULL then simply uses the ages as
  recorded.

- sdFactor:

  The number of standard deviations to include in the low and high
  calculations. The default is 1.0.

- dopng:

  Deprecated with nwfscSurvey 2.1 because providing a non-NULL value to
  `dir` can serve the same purpose as `dopng = TRUE` without the
  potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
  no longer have to specify `dopng` to save the plot as a png.

## See also

[`fit_vbgrowth()`](fit_vbgrowth.md)

## Author

Chantel Wetzel
