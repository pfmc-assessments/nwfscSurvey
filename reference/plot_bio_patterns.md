# This function plots length by latitude and depth

This function plots length by latitude and depth

## Usage

``` r
plot_bio_patterns(
  data,
  dir = NULL,
  col_name = "Length_cm",
  plot = 1:3,
  width = 7,
  height = 7
)
```

## Arguments

- data:

  Biological data frame from [`pull_bio()`](pull_bio.md)

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- col_name:

  Option to switch between plotting lengths or ages. Options are
  "Length_cm", "Width_cm", or "Age".

- plot:

  A vector of integers to specify which plots you would like. The
  default is to print or save both figures, i.e., `plot = 1:3`. Integers
  correspond to the following figures:

  1.  length/age by latitude and depth

  2.  length/age by depth and year

  3.  length/age by lat and year

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 10 by 7 inches.

## See also

Other plot\_: [`PlotSexRatio.fn()`](PlotSexRatio.fn.md),
[`PlotVarLengthAtAge.fn()`](PlotVarLengthAtAge.fn.md),
[`plot_age_length_sampling()`](plot_age_length_sampling.md),
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
