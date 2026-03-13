# Plot variability of length at age

Plot variability of length at age

## Usage

``` r
plot_varlenage(
  dat,
  dir = NULL,
  main = NULL,
  Par = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.1, CV1 = 0.1),
  bySex = TRUE,
  estVB = TRUE,
  bins = NULL,
  legX = "bottomleft",
  legY = NULL,
  dopng = lifecycle::deprecated(),
  ...
)
```

## Arguments

- dat:

  The data loaded from [`pull_bio()`](pull_bio.md)

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- main:

  Name that will be used to name the saved png

- Par:

  Starting parameters for K, Linf, L0, CV0, and CV2 based on the Stock
  Synthesis parameterization of von Bertanlaffy growth.

- bySex:

  Logical to indicate if plot by sex

- estVB:

  Logical. Estimate vonB growth to plot against predicted length. If F,
  it uses the parameters in `parStart`.

- bins:

  The bins to put ages into. If NULL then simply uses the ages as
  recorded.

- legX:

  legend location for x axis, defaults to "bottomleft"

- legY:

  legend location for y axis, defaults to NULL

- dopng:

  Deprecated with nwfscSurvey 2.1 because providing a non-NULL value to
  `dir` can serve the same purpose as `dopng = TRUE` without the
  potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
  no longer have to specify `dopng` to save the plot as a png.

- ...:

  Additional arguments for the plots.

## Details

Plots the standard deviation and coefficient of variation of age at
observed and predicted length

## See also

Other plot\_: [`PlotSexRatio.fn()`](PlotSexRatio.fn.md),
[`PlotVarLengthAtAge.fn()`](PlotVarLengthAtAge.fn.md),
[`plot_comps()`](plot_comps.md), [`plot_cpue()`](plot_cpue.md),
[`plot_cpue_map()`](plot_cpue_map.md), [`plot_index()`](plot_index.md),
[`plot_length_age()`](plot_length_age.md),
[`plot_proportion()`](plot_proportion.md),
[`plot_sex_ratio()`](plot_sex_ratio.md),
[`plot_sex_ratio_strata()`](plot_sex_ratio_strata.md),
[`plot_weight_length()`](plot_weight_length.md),
[`plot_westcoast()`](plot_westcoast.md)

## Author

Chantel Wetzel
