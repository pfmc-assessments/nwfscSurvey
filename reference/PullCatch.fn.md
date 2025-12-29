# Pull catch data from the NWFSC data warehouse

Pull catch data from the [NWFSC data
warehouse](https://www.webapps.nwfsc.noaa.gov/data) for a single species
or all observed species, where the latter is specified by leaving both
`Name = NULL` and `SciName = NULL`.

## Usage

``` r
PullCatch.fn(
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

  A character entry with the desired common name of the species you want
  to pull data for from the data warehouse. Use a vector of names if you
  want information for more than one species or if the desired species
  is included in the database using more than one name, e.g., vermilion
  rockfish (see the example below). Use the `SciName` argument if you
  know the latin name.

- SciName:

  A character entry with the desired scientific name of the species you
  want to pull data for from the data warehouse. Use a vector of names
  if you want information for more than one species or if the desired
  species is included in the database using more than one name, e.g.,
  vermilion rockfish (see the example below). Use the `Name` argument if
  you know the common name.

- YearRange:

  An integer vector of length two with the range of years to pull data
  for.

- SurveyName:

  A character entry from one of the following options that specifies
  which survey to pull the data for:

  - Triennial,

  - AFSC.Slope,

  - NWFSC.Combo,

  - NWFSC.Slope,

  - NWFSC.Shelf,

  - NWFSC.Hypoxia,

  - NWFSC.Santa.Barb.Basin,

  - NWFSC.Shelf.Rockfish (not yet working),

  - NWFSC.Hook.Line (not yet working),

  - NWFSC.Video,

  - Triennial.Canada

  Currently, you must pull data one survey at a time, though we are
  working on allowing for a vector of survey names and
  `NWFSC.Shelf.Rockfish` and `NWFSC.Hook.Line` are not supported. The
  default of `NULL` is a placeholder that must be replaced with an
  entry.

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

## Details

The data available in the warehouse are cleaned prior to being
downloaded with the intent that they provide the best available
information for use in an index-standardization procedure. The removed
samples may be of use to others with a less-restrictive goal than
producing an index of abundance. For example, life-stage samples are
excluded because they are not collected using the same protocols as
standard samples. To download all data, we currently recommend going to
the [NWFSC data warehouse](https://www.webapps.nwfsc.noaa.gov/data) and
using the csv link to extract data for a single species at a time. In
the future, we hope to add functionality to this package such that
downloading all data can be done easily within this function. See [Issue
\#43](https://github.com/pfmc-assessments/nwfscSurvey/issues/43) for
more information.

## Author

Chantel Wetzel (maintainer) based on code by John Wallace

## Examples

``` r
if (FALSE) { # \dontrun{
# SurveyName is only arg that has to be specified
dat <- PullCatch.fn(SurveyName = "NWFSC.Combo")

# Example with specified common name
catch_dat <- PullCatch.fn(
  Name = "vermilion rockfish",
  SurveyName = "NWFSC.Combo"
)

# Example with specified scientific name
catch_dat <- PullCatch.fn(
  SciName = "Eopsetta jordani",
  SurveyName = "NWFSC.Combo"
)

# Example with multiple names
catch_dat <- PullBio.fn(Name = c(
  "vermilion rockfish",
  "vermilion and sunset rockfish"
), SurveyName = "NWFSC.Combo")

} # }
```
