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

  Directory to save files to

- bio:

  Data biological sample file

- col_name:

  Option to switch between plotting lengths or ages. Options are
  "Length_cm", "Width_cm", or "Age".

- plot:

  A vector of integers specifying the figures you want.

- width:

  Numeric figure width in inches, defaults to 7

- height:

  Numeric figure height in inches, defaults to 7

## Author

Chantel Wetzel
