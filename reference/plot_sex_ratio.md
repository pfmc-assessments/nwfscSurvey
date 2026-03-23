# Function to plot sex ratio data in a barplot form

Function to plot sex ratio data in a barplot form

## Usage

``` r
plot_sex_ratio(
  data,
  dir = NULL,
  comp_column_name = "length_cm",
  main = NULL,
  bin_width = ifelse(comp_column_name == "length_cm", 2, 1),
  width = 7,
  height = 7
)
```

## Arguments

- data:

  Data object with biological data from [`pull_bio()`](pull_bio.md) with
  a column named `Sex` or `sex` is present.

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- comp_column_name:

  The name of the column to create composition data for that must be a
  string. This column can be is used to determine whether to format the
  composition data for length or age compositions by looking for either
  age (e.g., `age_years`, `Age`, `age`, `best_age`) or length (e.g.,
  `length`, `length_cm`, `Length`, `Length_cm`) in the comp_column_name.
  The comp_column_name is not case sensitive.The default is `length_cm`.

- main:

  Unique string that will be added to the saved png name. This can be
  useful when plotting data for various data sets separately.

- bin_width:

  Width to bin the data by. The default is 2 cm for lengths and 1 for
  ages.

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
[`plot_sex_ratio_strata()`](plot_sex_ratio_strata.md),
[`plot_var_length_at_age()`](plot_var_length_at_age.md),
[`plot_weight_length()`](plot_weight_length.md),
[`plot_westcoast()`](plot_westcoast.md)

## Author

Chantel Wetzel
