# Plot the United States west coast using ggplot2

Plot the United States west coast such that it can used as a standalone
figure or as a base to plot data points on.

## Usage

``` r
plot_westcoast()
```

## Value

A ggplot2 object.

## See also

- [`plot_cpue_map()`](plot_cpue_map.md) uses this function as the base
  map.

Other plot\_: [`PlotSexRatio.fn()`](PlotSexRatio.fn.md),
[`PlotVarLengthAtAge.fn()`](PlotVarLengthAtAge.fn.md),
[`plot_comps()`](plot_comps.md), [`plot_cpue()`](plot_cpue.md),
[`plot_cpue_map()`](plot_cpue_map.md), [`plot_index()`](plot_index.md),
[`plot_length_age()`](plot_length_age.md),
[`plot_proportion()`](plot_proportion.md),
[`plot_sex_ratio()`](plot_sex_ratio.md),
[`plot_sex_ratio_strata()`](plot_sex_ratio_strata.md),
[`plot_varlenage()`](plot_varlenage.md),
[`plot_weight_length()`](plot_weight_length.md)

## Author

Kelli F. Johnson

## Examples

``` r
if (FALSE) { # \dontrun{
if (require("maps") && require("mapproj")) {
  map_object <- plot_westcoast()
  print(map_object)
  ggplot2::ggsave(
    filename = "myfilename.png",
    plot = map_object
  )
}
} # }
```
