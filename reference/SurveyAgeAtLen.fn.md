# Calculates conditional age-at-length composition data

This function has been deprecated as of version 2.6. Please use
get_raw_caal().

## Usage

``` r
SurveyAgeAtLen.fn(
  dir = NULL,
  datAL,
  datTows,
  strat.vars = c("Depth_m", "Latitude_dd"),
  strat.df = NULL,
  lgthBins = 1,
  ageBins = 1,
  sex = 3,
  SSout = TRUE,
  meanRatioMethod = TRUE,
  raw = TRUE,
  NAs2zero = TRUE,
  month = "Enter Month",
  fleet = "Enter Fleet",
  partition = 0,
  ageerr = "Enter Numeric",
  ageErr = lifecycle::deprecated(),
  returnSamps = FALSE,
  printfolder = "forSS3",
  verbose = TRUE
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- datAL:

  the biological data frame extracted from the data warehouse using
  [`pull_bio()`](pull_bio.md)

- datTows:

  the catch data frame extracted from the data warehouse using
  [`pull_catch()`](pull_catch.md)

- strat.vars:

  Variables in both data frame that are used to define the strata.
  Default is bottom depth (m) and latitudes (decimal degrees), i.e.,
  `c("Depth_m", "Latitude_dd")`.

- strat.df:

  A data frame that defines the strata and provides the calculated areas
  for each strata returned from `createStrataDF.fn()`.

- lgthBins:

  Vector of length bins to create length compositions across. Values
  above or below the minimum or maximum values, respectively, are
  grouped into the first size or plus group size.

- ageBins:

  Vector of age bins to create age compositions across. Values above or
  below the minimum or maximum values, respectively, are grouped into
  the first age or plus group age.

- sex:

  Options of (0, 1, 2, 3). The integer will be used to define the sex
  column of the returned input for Stock Synthesis and specifies how the
  composition are treated with respect to sex. See the Stock Synthesis
  manual for more information. In short, 0 is for unsexed, 1 is females,
  2 is males, and 3 is females and males. The default is `3`.

- SSout:

  A logical with the default of `TRUE`. If `TRUE`, the output is
  returned in a format that can be directly pasted into an SS3 data
  file.

- meanRatioMethod:

  A logical with the default of `TRUE`. If `TRUE`, then the mean ratio
  is implemented instead of the total ratio. Search the source code for
  the equations if more information is needed.

- raw:

  Logical input to define whether or not to expand numbers in the csv
  file with a default unexpanded sample numbers.

- NAs2zero:

  A logical specifying if `NA`s should be changed to zeros. The default
  is `TRUE`.

- month:

  Month the samples were collected based on the expected format for
  Stock Synthesis to determine the length/age estimate to compare to.
  Default "Enter Month".

- fleet:

  A fleet number to assign the composition data to based on the expected
  format for Stock Synthesis. Default "Enter Fleet".

- partition:

  Partition to assign the composition data based on the expected format
  for Stock Synthesis. Partition of 0 indicates that the composition
  data include all composition data, 1 for discarded composition data,
  and 2 for retained fish only. Default of 0.

- ageerr:

  Number of ageing error vector to apply to the age data based on Stock
  Synthesis. Default "Enter Numeric".

- ageErr:

  Deprecated.

- returnSamps:

  A logical with the default of `FALSE`. A value of `TRUE` stops the
  function after the sample size is calculated.

- printfolder:

  A string that will be appended to `dir`, creating a folder where the
  output will be saved. If specified as `""`, the output will just be
  saved directly in `dir`. The default is `"forSS3"`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## See also

[`StrataFactors.fn`](StrataFactors.fn.md)

## Author

Allan Hicks and Chantel Wetzel
