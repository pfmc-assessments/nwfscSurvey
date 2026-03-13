# This function plots length by latitude and depth

This function plots length by latitude and depth

## Usage

``` r
plot_bio_patterns(
  dir = NULL,
  bio,
  col_name = "Length_cm",
  plot = 1:3,
  width = 7,
  height = 7
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- bio:

  Biological data frame from [`pull_bio()`](pull_bio.md)

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

## Author

Chantel Wetzel
