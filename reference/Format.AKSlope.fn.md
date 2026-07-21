# Deprecated function replaced by pull_catch() and pull_bio()

Deprecated function replaced by pull_catch() and pull_bio()

## Usage

``` r
Format.AKSlope.fn(
  dir = NULL,
  datTows,
  datL = NA,
  start.year = 1997,
  verbose = TRUE
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- datTows:

  A data frame of catch data for the AKFSC slope survey with incorrect
  column names. prior to the creation of the data warehouse.

- datL:

  A list of biological data (lengths and ages) for the AKFSC slope
  survey with incorrect column names prior to the creation of the data
  warehouse.

- start.year:

  The first year of data to retain within the data frame. The first year
  typically used from this survey is 1997.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Details

This function is no longer used. Please use pull_catch() and pull_bio()
to get properly formatted and filtered data.

## Author

Chantel Wetzel
