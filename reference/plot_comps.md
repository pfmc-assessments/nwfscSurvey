# Plot frequency data as bubble plots

Plot frequency data as bubble plots

## Usage

``` r
plot_comps(
  data,
  dir = NULL,
  add_save_name = NULL,
  plot = 1:2,
  add_0_ylim = TRUE,
  width = 10,
  height = 7
)
```

## Arguments

- data:

  Data file object created by
  [`get_expanded_comps()`](get_expanded_comps.md) or
  [`get_raw_comps()`](get_raw_comps.md).

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- add_save_name:

  Option to add text to a saved figure name. This option can be useful
  if creating plots across multiple species and saving them into a
  single folder. The default is `NULL`. Note that the data type, i.e.,
  age or length, and sex type are already included in the saved name so
  no need to add those here.

- plot:

  A vector of integers to specify which plots you would like. The
  default is to print or save both figures, i.e., `plot = 1:2`. Integers
  correspond to the following figures:

  1.  bubble plot of length-/age-composition data by year and sex and

  2.  distribution by year of length-/age-compositions data similar to
      the r4ss figure.

- add_0_ylim:

  A logical, i.e., `TRUE`/`FALSE`, argument that specifies if the y axis
  should start at 0. If `FALSE`, the y axis will start at the minimum
  bin size used in data. The default is TRUE. This currently only
  pertains to plot 1, not plot 2.

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 10 by 7 inches.

## See also

Other plot\_: [`PlotSexRatio.fn()`](PlotSexRatio.fn.md),
[`PlotVarLengthAtAge.fn()`](PlotVarLengthAtAge.fn.md),
[`plot_age_length_sampling()`](plot_age_length_sampling.md),
[`plot_bio_patterns()`](plot_bio_patterns.md),
[`plot_cpue()`](plot_cpue.md), [`plot_cpue_map()`](plot_cpue_map.md),
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

## Examples

``` r
if (FALSE) { # \dontrun{
catch <- pull_catch(common_name = "lingcod")
bio <- pull_bio(common_name = "lingcod")

strata <- create_strata(
  names = c("deep_s", "shallow_s", "deep_mn", "shallow_m", "shallow_n"),
  depths_shallow = c(183, 55, 183, 55, 55),
  depths_deep = c(549, 183, 549, 183, 183),
  lats_south = c(32, 32, 40, 40, 44),
  lats_north = c(40, 40, 49, 44, 49)
)
length_comps <- get_expanded_comps(
  bio_data = bio,
  catch_data = catch,
  comp_bins = seq(10, 40, 2),
  strata = strata
)
plot_comps(data = length_comps)
} # }
```
