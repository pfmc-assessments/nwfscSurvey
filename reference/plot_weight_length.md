# Plots the weight-at-length data and estimates

Plots the weight-at-length data and estimates

## Usage

``` r
plot_weight_length(
  data,
  dir = NULL,
  estimates = NULL,
  col_length = "length_cm",
  col_weight = "weight_kg",
  two_sex = TRUE,
  add_save_name = NULL,
  height = 7,
  width = 7,
  dpi = 300
)
```

## Arguments

- data:

  Data frame created by [`pull_bio()`](pull_bio.md)

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- estimates:

  Data frame of weight-at-length estimates from `est_weight_length()`.
  If passed to the function the estimated parameter values will be added
  to the plot. The default is `NULL` and will not add parameters to the
  plot.

- col_length:

  A numeric or character value specifying the column to use in `data`
  for length information. These lengths are assumed to be in
  centimeters. The default value is `length_cm`.

- col_weight:

  A numeric or character value specifying the column to use in `data`
  for weight information. These weights are assumed to be in kilograms
  The default value is `weight_kg`. Using kilograms is the default
  because Stock Synthesis assumes the weight-length parameters are
  calculated using centimeters and kilograms. The reported values are
  easily scaled to give you results in grams if you wish to have more
  standard parameter estimates.

- two_sex:

  Default TRUE. If TRUE the sexed data will be plotted and if FALSE all
  data will be plotted as unsexed. If `estimates` are passed to the
  function then the corresponding parameter estimates will be plotted.

- add_save_name:

  Option to add text to a saved figure name. This option can be useful
  if creating plots across multiple species and saving them into a
  single folder. The default is `NULL`. Note that the biomass estimate,
  i.e., annual or strata, are already included in the saved name so no
  need to add those here.

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 7 by 7 inches.

- dpi:

  The resolution to apply when saving figures. Lower resolution values
  can reduce file size which can be helpful when creating large
  documents with many figures. The default is 300.

## Author

Chantel Wetzel
