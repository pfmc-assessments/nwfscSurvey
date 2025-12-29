# Rename AFSC slope survey columns from data pulled before 2017

Rename AFSC slope survey columns from data pulled before 2017

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

Rename columns in the AFSC slope survey data file received prior to the
creation of the NWFSC data warehouse. This function converts the older
data files to create the needed column names to work within survey
package functions. Output from this function will be list of containing
catch, length, and age data.

## Author

Chantel Wetzel

## Examples

``` r
if (FALSE) { # \dontrun{
# load data files for catch and biological data
load("Tri.Shelf.and.AFSC.Slope.canary.Catch.24.May.11.dmp")
catch <- Tri.Shelf.and.AFSC.Slope.canary.Catch.24.May.11
load("AFSC.Slope.Shelf.sable.bio.5.24.11.dmp")
bio <- AK.Surveys.Bio.sablefish.24.May.11
# call function and reformat the data
filter.dat <- Format.AKSlope.fn(
  datTows = catch,
  datL = bio,
  start.year = 1997
)
} # }
```
