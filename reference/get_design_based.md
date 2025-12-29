# Calculates design based estimates from survey data for West Coast surveys.

Calculates design based estimates from survey data for West Coast
surveys.

## Usage

``` r
get_design_based(
  data,
  strata,
  CI = 0.95,
  dir = NULL,
  month = NA,
  fleet = NA,
  printfolder = "forSS3",
  verbose = TRUE
)
```

## Arguments

- data:

  Data frame of catch data that has been created by the
  [`pull_catch()`](pull_catch.md).

- strata:

  A data frame that defines the strata and provides the calculated areas
  for each strata returned from `createStrataDF.fn()`.

- CI:

  A numerical value that specifies the confidence interval to return.
  Values should be between 0.01 to 0.99.

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- month:

  A single integer value between 1-12. A user input fleet number to
  assign to the month column based on the expected format for Stock
  Synthesis. See the Stock Synthesis manual for more information.
  Default "Enter Month".

- fleet:

  A single integer value. A user input fleet number to assign to the
  fleet column based on the expected format for Stock Synthesis. Default
  "Enter Fleet".

- printfolder:

  A string that will be appended to `dir`, creating a folder where the
  output will be saved. If specified as `""`, the output will just be
  saved directly in `dir`. The default is `"forSS3"`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

List of biomass estimates by year and biomass estimates by year and
strata. The biomass estimates are in metric tons.

## Details

The design based index is calculated based on the area of the strata
with the output estimates representing the adjusted median estimates
(e.g., est \* exp(0.5\*log(var)). This function returns a list of
design-based estimates by strata and estimates combined across strata by
year. This function is designed to work with data frames pulled from the
NWFSC data warehouse using [`pull_catch()`](pull_catch.md). See:
Gunderson, D.R. and Sample, T.M. 1980. Distribution and abundance of
rockfish off Washington, Oregon, and California during 1977. Marine
Fisheries Review: March - April.

## Author

Allan Hicks and Chantel Wetzel

## Examples

``` r
if (FALSE) { # \dontrun{
catch <- pull_catch(
  common_name = "petrale sole",
  survey = "NWFSC.Combo"
)

strata <- CreateStrataDF.fn(
  names = c("shallow_wa", "shallow_or", "shallow_ca", "deep_wa", "deep_or", "deep_ca"),
  depths.shallow = c(55, 55, 55, 183, 183, 183),
  depths.deep = c(183, 183, 183, 549, 549, 549),
  lats.south = c(46.0, 42.0, 32.0, 46.0, 42.0, 32.0),
  lats.north = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0)
)

biommass <- get_design_based(
  data = catch,
  strata = strata
)
} # }
```
