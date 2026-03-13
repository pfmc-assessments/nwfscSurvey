# Calculate the number of observations by year and strata

Provides a summary of total and positive tows across different strata
and years to help evaluate the suitability of the selected
stratification.

## Usage

``` r
check_strata(
  data,
  strata,
  dir = NULL,
  printfolder = lifecycle::deprecated(),
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame of catch data created by [`pull_catch()`](pull_catch.md).

- strata:

  A data frame that defines the strata and provides the calculated areas
  for each strata returned from [`create_strata()`](create_strata.md).

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- printfolder:

  Deprecated with nwfscSurvey 2.8.0 to give users greater control on
  where to save files. A string that will be appended to `dir`, creating
  a folder where the output will be saved.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

A matrix containing the total number of tows and positive tows
cross-tabulated by year and strata.

## Details

Calculates and returns the total number of tows and positive tows
conducted in each strata by year. The selected strata are used to expand
the length and marginal age compositions and to calculate a design-based
index using [`get_design_based()`](get_design_based.md).

In earlier versions of the code, more than one positive observation
within each strata was required to calculate a design-based index. The
current [`get_design_based()`](get_design_based.md) function is more
robust and returns zeros in strata-year combinations with no
observations. However, reviewing the frequency of tows and positive tows
is recommended to ensure that the selected strata are reasonable (e.g.,
avoiding limited observations in large areas).

## See also

Other helper functions: [`codify_sex()`](codify_sex.md)

## Author

Chantel Wetzel
