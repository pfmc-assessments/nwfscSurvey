# Pull haul data from the NWFSC data warehouse The website is: https://www.webapps.nwfsc.noaa.gov/data This function can be used to pull haul data and associated covariates

Pull haul data from the NWFSC data warehouse The website is:
https://www.webapps.nwfsc.noaa.gov/data This function can be used to
pull haul data and associated covariates

## Usage

``` r
PullHaul.fn(
  YearRange = c(1980, 5000),
  SurveyName = NULL,
  SaveFile = lifecycle::deprecated(),
  Dir = NULL,
  verbose = TRUE
)
```

## Arguments

- YearRange:

  range of years to pull data. Defaults to all years, 1977 - present.

- SurveyName:

  survey to pull the data for the options are: Triennial, AFSC.Slope,
  NWFSC.Combo, NWFSC.Slope, NWFSC.Shelf, NWFSC.Hypoxia,
  NWFSC.Santa.Barb.Basin, NWFSC.Shelf.Rockfish (NWFSC.Hook.Line but both
  are not working), NWFSC.Video#'

- SaveFile:

  Deprecated with nwfscSurvey 2.3. Output will be save automatically if
  the Dir input is specified.

- Dir:

  directory where the file should be saved

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

Returns a data frame of haul characteristics for satisfactory hauls

## Author

Eric Ward

## Examples

``` r
if (FALSE) { # \dontrun{
haul_dat <- PullHaul.fn(SurveyName = "NWFSC.Combo", YearRange = c(2003, 2007))
haul_dat <- PullHaul.fn()
} # }
```
