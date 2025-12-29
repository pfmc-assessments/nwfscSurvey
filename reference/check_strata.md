# Calculate the number of observations by year and strata

Calculate the number of observations by year and strata

## Usage

``` r
check_strata(data, strata, dir = NULL, printfolder = "forSS3", verbose = TRUE)
```

## Arguments

- data:

  Data frame of catch data that has been created by the
  [`pull_catch()`](pull_catch.md).

- strata:

  A data frame that defines the strata and provides the calculated areas
  for each strata returned from `createStrataDF.fn()`.

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- printfolder:

  A string that will be appended to `dir`, creating a folder where the
  output will be saved. If specified as `""`, the output will just be
  saved directly in `dir`. The default is `"forSS3"`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

A matrix with the number of tows and positive tows within each strata by
year and the number of positive tows by strata and year.

## Details

Calculates and returns the total number of tows and positive tows
conducted in each strata by year. The selected strata are used to expand
the length and marginal age compositions and to calculate a design based
index using the [`get_design_based()`](get_design_based.md). In earlier
versions of the code, there needed to be more than one positive
observations within each strata to calculate a design based index using
[`Biomass.fn()`](Biomass.fn.md). The new
[`get_design_based()`](get_design_based.md) function is more robust and
will return zeros in each strata-year combination with no observations.
However, it can be useful to review how many tows and positive tows are
present by year and strata in your data to ensure that the selected
strata for expanding the data is reasonable (e.g., avoiding limited
observations in large areas).

## Author

Chantel Wetzel
