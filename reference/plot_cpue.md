# This function plots cpue and length by latitude and depth

This function plots cpue and length by latitude and depth

## Usage

``` r
plot_cpue(data, dir = NULL, plot = 1:3, width = 7, height = 7)
```

## Arguments

- data:

  Data catch file pulled using [`pull_catch()`](pull_catch.md)

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- plot:

  A vector of integers to specify which plots to return. The default is
  to print or save all figures, i.e., `plot = 1:3`. Integers correspond
  to the following figures:

  1.  log(CPUE) by depth & log(CPUE) by latitude

  2.  log(CPUE) by latitude and year

  3.  log(CPUE) by depth and year

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 7 by 7 inches.

## See also

Other plot\_: [`PlotSexRatio.fn()`](PlotSexRatio.fn.md),
[`PlotVarLengthAtAge.fn()`](PlotVarLengthAtAge.fn.md),
[`plot_age_length_sampling()`](plot_age_length_sampling.md),
[`plot_bio_patterns()`](plot_bio_patterns.md),
[`plot_comps()`](plot_comps.md), [`plot_cpue_map()`](plot_cpue_map.md),
[`plot_index()`](plot_index.md),
[`plot_length_age()`](plot_length_age.md),
[`plot_proportion()`](plot_proportion.md),
[`plot_sex_ratio()`](plot_sex_ratio.md),
[`plot_sex_ratio_strata()`](plot_sex_ratio_strata.md),
[`plot_var_length_at_age()`](plot_var_length_at_age.md),
[`plot_weight_length()`](plot_weight_length.md),
[`plot_westcoast()`](plot_westcoast.md)

## Author

Chantel Wetzel
