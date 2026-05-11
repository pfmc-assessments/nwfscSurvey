# Plot the representativeness of age sampling based on lengths

Plot the representativeness of age sampling based on lengths

## Usage

``` r
plot_age_length_sampling(
  data,
  dir = NULL,
  plot = 1:2,
  height = 7,
  width = 7,
  verbose = TRUE,
  xlim = lifecycle::deprecated(),
  ylim = lifecycle::deprecated()
)
```

## Arguments

- data:

  A data frame of composition data returned from
  [`pull_bio()`](pull_bio.md).

- dir:

  Defaults to NULL (plot made, but not saved to file). Can alternatively
  be a string filename by year and sex.

- plot:

  A vector of integers to specify which plots to return. The default is
  to print or save all figures, i.e., `plot = 1:2`. Integers correspond
  to the following figures:

  1.  Compare distribution of all lengths and lengths from aged fish
      across all years

  2.  Compare distribution of all lengths and lengths from aged fish
      across by year

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 7 by 7 inches.

- xlim:

  Deprecated as of v.2.8.1. x limits for plot, defaults to c(0,120)

- ylim:

  Deprecated as of v.2.8.1. y limits for plot, defaults to (0, 0.049)

## See also

Other plot\_: [`PlotSexRatio.fn()`](PlotSexRatio.fn.md),
[`PlotVarLengthAtAge.fn()`](PlotVarLengthAtAge.fn.md),
[`plot_bio_patterns()`](plot_bio_patterns.md),
[`plot_comps()`](plot_comps.md), [`plot_cpue()`](plot_cpue.md),
[`plot_cpue_map()`](plot_cpue_map.md), [`plot_index()`](plot_index.md),
[`plot_length_age()`](plot_length_age.md),
[`plot_proportion()`](plot_proportion.md),
[`plot_sex_ratio()`](plot_sex_ratio.md),
[`plot_sex_ratio_strata()`](plot_sex_ratio_strata.md),
[`plot_var_length_at_age()`](plot_var_length_at_age.md),
[`plot_weight_length()`](plot_weight_length.md),
[`plot_westcoast()`](plot_westcoast.md)

## Author

Chantel Wetzel
