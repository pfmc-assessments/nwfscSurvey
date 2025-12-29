# Calculate the number of observations by year and strata

Calculate the number of observations by year and strata

## Usage

``` r
CheckStrata.fn(
  dir = NULL,
  dat,
  strat.vars = c("Depth_m", "Latitude_dd"),
  strat.df,
  printfolder = "forSS3",
  verbose = TRUE
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- dat:

  Data-frame of the catch data that has been created by
  [`pull_catch()`](pull_catch.md).

- strat.vars:

  Variables in both data frame that are used to define the strata.
  Default is bottom depth (m) and latitudes (decimal degrees), i.e.,
  `c("Depth_m", "Latitude_dd")`.

- strat.df:

  A data frame that defines the strata and provides the calculated areas
  for each strata returned from `createStrataDF.fn()`.

- printfolder:

  A string that will be appended to `dir`, creating a folder where the
  output will be saved. If specified as `""`, the output will just be
  saved directly in `dir`. The default is `"forSS3"`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

A matrix with the number of tows within each strata by year and the
number of positive tows by strata and year.

## Details

Calculates and returns the total number of tows and positive tows
conducted in each strata by year. The selected strata are used to expand
the length and marginal age compositions and to calculate a design based
index using the [`Biomass.fn()`](Biomass.fn.md).

## Author

Chantel Wetzel
