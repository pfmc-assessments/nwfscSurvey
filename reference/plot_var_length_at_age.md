# Plot variability of length at age

Plot variability of length at age

## Usage

``` r
plot_var_length_at_age(
  data,
  age_bins,
  dir = NULL,
  main = NULL,
  two_sex = TRUE,
  height = 7,
  width = 7
)
```

## Arguments

- data:

  A data frame of length-composition data returned from
  [`pull_bio()`](pull_bio.md).

- age_bins:

  Vector of integers to bin age data.Values above or below the minimum
  or maximum values in the vector are grouped into the first size or
  plus group size, respectively.

- dir:

  Defaults to NULL (plot made, but not saved to file). Can alternatively
  be a string filename by year and sex.

- main:

  Name that will be used to name the saved png

- two_sex:

  Logical to indicate if plot by sex. Default is TRUE and will only plot
  females and males.

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 7 by 7 inches.

## See also

Other plot\_: [`PlotSexRatio.fn()`](PlotSexRatio.fn.md),
[`PlotVarLengthAtAge.fn()`](PlotVarLengthAtAge.fn.md),
[`plot_age_length_sampling()`](plot_age_length_sampling.md),
[`plot_bio_patterns()`](plot_bio_patterns.md),
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
