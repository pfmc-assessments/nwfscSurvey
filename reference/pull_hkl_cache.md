# Pull NWFSC Hook-and-Line Survey data from pinned data list on posit connect

This function can be used to return pinned data for NWFSC Hook-and-Line
Survey. If a `common_name` is provided the data will be formatted and
filtered for that species. A list is returned with catch and biological
data.

## Usage

``` r
pull_hkl_cache(
  years = c(1970, 2050),
  common_name = NULL,
  dir = NULL,
  return_positive_sites_only = TRUE,
  verbose = TRUE
)
```

## Arguments

- years:

  An integer vector of length two with the range of years to pull data
  for (e.g., c(2003, 2024)). Vector can not contain -Inf or Inf.

- common_name:

  A character entry with the desired common name of the species you want
  to pull data for from the data warehouse. Use a vector of names if you
  want information for more than one species or if the desired species
  is included in the database using more than one name, e.g., vermilion
  rockfish (see the example below). Use the `sci_name` argument if you
  know the latin name.

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- return_positive_sites_only:

  Logical that specifies whether data from sites that have observed
  common_name at least once across the time series are returned.The
  default is `TRUE`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

Returns a list of hook-and-line catch and biological data

## See also

Other data pulling functions: [`pull_bio()`](pull_bio.md),
[`pull_bio_cache()`](pull_bio_cache.md),
[`pull_biological_samples()`](pull_biological_samples.md),
[`pull_catch()`](pull_catch.md),
[`pull_catch_cache()`](pull_catch_cache.md),
[`pull_gemm()`](pull_gemm.md), [`pull_haul()`](pull_haul.md),
[`pull_haul_cache()`](pull_haul_cache.md)

## Author

Chantel Wetzel
