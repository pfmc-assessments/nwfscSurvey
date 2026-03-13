# Plots the design based biomass estimates with confidence intervals

Plots the design based biomass estimates with confidence intervals

## Usage

``` r
plot_index(
  data,
  dir = NULL,
  add_save_name = NULL,
  plot = 1:2,
  height = 7,
  width = 7,
  dpi = 300
)
```

## Arguments

- data:

  List of design based biomass estimates created by the
  [`get_design_based()`](get_design_based.md)

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- add_save_name:

  Option to add text to a saved figure name. This option can be useful
  if creating plots across multiple species and saving them into a
  single folder. The default is `NULL`. Note that the biomass estimate,
  i.e., annual or strata, are already included in the saved name so no
  need to add those here.

- plot:

  A vector of integers to specify which plots you would like. The
  default is to print or save both figures, i.e., `plot = 1:2`. Integers
  correspond to the following figures:

  1.  Design based index by year.

  2.  Design based index by year and strata.

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 7 by 7 inches.

- dpi:

  The resolution to apply when saving figures. Lower resolution values
  can reduce file size which can be helpful when creating large
  documents with many figures. The default is 300.

## Details

Plots both the design based biomass estimates by year with confidence
intervals and the design based biomass estimates by year and strata with
no confidence intervals.

## See also

Other plot\_: [`PlotSexRatio.fn()`](PlotSexRatio.fn.md),
[`PlotVarLengthAtAge.fn()`](PlotVarLengthAtAge.fn.md),
[`plot_comps()`](plot_comps.md), [`plot_cpue()`](plot_cpue.md),
[`plot_cpue_map()`](plot_cpue_map.md),
[`plot_length_age()`](plot_length_age.md),
[`plot_proportion()`](plot_proportion.md),
[`plot_sex_ratio()`](plot_sex_ratio.md),
[`plot_sex_ratio_strata()`](plot_sex_ratio_strata.md),
[`plot_varlenage()`](plot_varlenage.md),
[`plot_weight_length()`](plot_weight_length.md),
[`plot_westcoast()`](plot_westcoast.md)

## Author

Chantel Wetzel
