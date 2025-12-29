# Plots the length-at-age data and estimates

Plots the length-at-age data and estimates

## Usage

``` r
plot_length_age(
  data,
  dir = NULL,
  col_length = "length_cm",
  col_age = "age",
  estimates = NULL,
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

- col_length:

  A numeric or character value specifying the column to use in `data`
  for length information. These lengths are assumed to be in
  centimeters. The default value is `length_cm`.

- col_age:

  A numeric or character value specifying the column to use in `data`
  for age information. These ages are assumed to be in years. The
  default value is `age`.

- estimates:

  Data frame of age-at-length estimates from
  [`est_vbgrowth()`](est_vbgrowth.md). If passed to the function the
  estimated parameter values will be added to the plot. The default is
  `NULL` and will not add parameters to the plot.

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
