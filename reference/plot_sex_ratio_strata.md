# Function to plot sex ratio by strata

Function to plot sex ratio by strata

## Usage

``` r
plot_sex_ratio_strata(
  data,
  strata_df,
  dir = NULL,
  comp_column_name = "length_cm",
  strata_vars = c("depth_m", "latitude_dd"),
  bin_width = ifelse(comp_column_name == "length_cm", 2, 1),
  main = NULL,
  height = 7,
  width = 7
)
```

## Arguments

- data:

  Data object with biological data from [`pull_bio()`](pull_bio.md) with
  a column named `Sex` or `sex` is present.

- strata_df:

  Strata dataframe created by [`create_strata()`](create_strata.md)

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

- strata_vars:

  Column names in `data` to define the stratas by. The default columns
  are `Depth_m` or `depth_m` and `Latitude_dd` or `latitude_dd`.

- bin_width:

  Width to bin the data by. The default is 2 cm for lengths and 1 for
  ages.

- main:

  Unique string that will be added to the saved png name. This can be
  useful when plotting data for various data sets separately.

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 7 by 7 inches.

## See also

[`strata_factors`](strata_factors.md)
[`create_strata`](create_strata.md)

Other plot\_: [`PlotSexRatio.fn()`](PlotSexRatio.fn.md),
[`PlotVarLengthAtAge.fn()`](PlotVarLengthAtAge.fn.md),
[`plot_comps()`](plot_comps.md), [`plot_cpue()`](plot_cpue.md),
[`plot_cpue_map()`](plot_cpue_map.md), [`plot_index()`](plot_index.md),
[`plot_length_age()`](plot_length_age.md),
[`plot_proportion()`](plot_proportion.md),
[`plot_sex_ratio()`](plot_sex_ratio.md),
[`plot_varlenage()`](plot_varlenage.md),
[`plot_weight_length()`](plot_weight_length.md),
[`plot_westcoast()`](plot_westcoast.md)

## Author

Chantel Wetzel
