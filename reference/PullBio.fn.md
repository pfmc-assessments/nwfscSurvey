# Pull biological data (age, length, weight) from the NWFSC data warehouse The website is: https://www.webapps.nwfsc.noaa.gov/data This function can be used to pull a single species or all observed species In order to pull all species leave Name = NULL and SciName = NULL

Pull biological data (age, length, weight) from the NWFSC data warehouse
The website is: https://www.webapps.nwfsc.noaa.gov/data This function
can be used to pull a single species or all observed species In order to
pull all species leave Name = NULL and SciName = NULL

## Usage

``` r
PullBio.fn(
  Name = NULL,
  SciName = NULL,
  YearRange = c(1980, 5000),
  SurveyName = NULL,
  SaveFile = lifecycle::deprecated(),
  Dir = NULL,
  verbose = TRUE
)
```

## Arguments

- Name:

  common name of species data to pull from the data warehouse

- SciName:

  scientific name of species data to pull from the data warehouse

- YearRange:

  range of years to pull data

- SurveyName:

  survey to pull the data for the options are: Triennial, AFSC.Slope,
  NWFSC.Combo, NWFSC.Slope, NWFSC.Shelf, NWFSC.Hypoxia,
  NWFSC.Santa.Barb.Basin, NWFSC.Shelf.Rockfish (NWFSC.Hook.Line but both
  are not working), NWFSC.Video#'

- SaveFile:

  Deprecated with nwfscSurvey 2.3. Output will be save automatically if
  the Dir input is specified.

- Dir:

  The directory where you want the output file to be saved. The name of
  the file within `Dir` will start with Catch\_ and end with .rdata.
  Default NULL which will not save an output file.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Author

Chantel Wetzel based on code by John Wallace

## Examples

``` r
if (FALSE) { # \dontrun{
# SurveyName is only arg that has to be specified
bio_dat <- PullBio.fn(SurveyName = "NWFSC.Combo")

# Example with specified common name
bio_dat <- PullBio.fn(
  Name = "vermilion rockfish",
  SurveyName = "NWFSC.Combo"
)

# Example with specified scientific name
bio_dat <- PullBio.fn(
  SciName = "Eopsetta jordani",
  SurveyName = "NWFSC.Combo"
)

# Example with multiple names
bio_dat <- PullBio.fn(
  SciName = c("Sebastes aurora", "Eopsetta jordani"),
  SurveyName = "NWFSC.Combo"
)
} # }
```
