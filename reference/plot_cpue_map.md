# Plot two figures showing catch-per-unit-effort data

Plot catch-per-unit-effort (CPUE) data across all years and by year and
save them to the disk. Figures are created using ggplot2.

## Usage

``` r
plot_cpue_map(
  data,
  dir = NULL,
  main = NULL,
  plot = 1:2,
  height = 10,
  width = 7
)
```

## Arguments

- data:

  A dataframe of catch data created by [`pull_catch()`](pull_catch.md).

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- main:

  A string that will be prepended to the name of the saved png (i.e.,
  "NWFSC" results in a file called "NWFSC_CPUE_Map.png").

- plot:

  A vector of integers to specify which plots you would like. The
  default is to print both figures.

  1.  coastwide data across all years

  2.  coastwide data by year

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 7 by 10 inches.

## Value

Figures are saved to the disk according to which plots are asked for in
`plot`. Each of the specified files are saved to `dir`, the specified
directory. No objects are returned to the user. But, the figures are
printed to new windows if they are not saved to the disk.

## See also

- [`pull_catch()`](pull_catch.md)

- [`plot_westcoast()`](plot_westcoast.md)

Other plot\_: [`PlotSexRatio.fn()`](PlotSexRatio.fn.md),
[`PlotVarLengthAtAge.fn()`](PlotVarLengthAtAge.fn.md),
[`plot_age_length_sampling()`](plot_age_length_sampling.md),
[`plot_bio_patterns()`](plot_bio_patterns.md),
[`plot_comps()`](plot_comps.md), [`plot_cpue()`](plot_cpue.md),
[`plot_index()`](plot_index.md),
[`plot_length_age()`](plot_length_age.md),
[`plot_proportion()`](plot_proportion.md),
[`plot_sex_ratio()`](plot_sex_ratio.md),
[`plot_sex_ratio_strata()`](plot_sex_ratio_strata.md),
[`plot_var_length_at_age()`](plot_var_length_at_age.md),
[`plot_weight_length()`](plot_weight_length.md),
[`plot_westcoast()`](plot_westcoast.md)

## Author

Chantel R. Wetzel

## Examples

``` r
if (FALSE) { # \dontrun{
plot_cpue_map(data = catch_nwfsc_combo, plot = 1)
plot_cpue_map(data = catch_nwfsc_combo, plot = 2)
} # }
```
