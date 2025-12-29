# Filter data pulls

Function to create messages on data that are outside the standard survey
protocol and to remove these samples if `standard_filtering` = TRUE. The
data are checked for tow performance, valid stations, and depth range.
This function is called within the pull functions, but can be called on
pulled data frames if filtering was not selected in the original data
pull.

## Usage

``` r
filter_pull(
  data,
  data_type = "samples",
  standard_filtering = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame of pulled data created by the
  [`pull_catch()`](pull_catch.md), [`pull_bio()`](pull_bio.md),
  [`pull_haul()`](pull_haul.md), or
  [pull_biological_samples](pull_biological_samples.md).

- data_type:

  Character string to include within data filtering messages to indicate
  the type of data being filtered such as "biological samples". Default
  is "samples".

- standard_filtering:

  A logical TRUE/FALSE that specifies whether data should be filtered
  using the standard filtering which removes tows with bad performance
  (water haul or poor net performance), or stations that have been
  removed from the survey sampling protocol.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Author

Chantel Wetzel
