# Format NWFSC Hook-and-Line catch data

This function will format hook-and-line catch to account for all
observations of the `common_name`, if provided. If
`return_positive_sites_only` is TRUE, the the data will be filtered down
remove sampling sites that have never observed the select species. If no
`common_name` is provided, then an unfiltered data set is returned.

## Usage

``` r
format_hkl_catch_data(
  data,
  common_name = NULL,
  return_positive_sites_only = TRUE,
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame of NWFSC Hook-and-Line data.

- common_name:

  Species names as given in the hook and line data set to format data
  for.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

- return_positive_site_only:

  Logical that specifies whether data from sites that have observed
  common_name at least once across the time series are returned.The
  default is `TRUE`.

## Value

Returns a data frame of formatted hook-and-line catch data frame

## Author

Chantel Wetzel
