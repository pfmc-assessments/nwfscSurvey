# Create marginal age composition data

Create marginal age composition data

## Usage

``` r
SurveyAFs.fn(
  dir = NULL,
  datA,
  datTows,
  strat.vars = c("Depth_m", "Latitude_dd"),
  strat.df = NULL,
  ageBins = 1,
  SSout = TRUE,
  meanRatioMethod = TRUE,
  sex = 3,
  NAs2zero = T,
  sexRatioUnsexed = NA,
  maxSizeUnsexed = NA,
  sexRatioStage = 1,
  partition = 0,
  fleet = "Enter Fleet",
  agelow = "Enter",
  agehigh = "Enter",
  ageErr = "Enter",
  nSamps = "Enter Samps",
  month = "Enter Month",
  printfolder = "forSS3",
  remove999 = TRUE,
  outputStage1 = FALSE,
  verbose = TRUE
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- datA:

  the biological data frame extracted from the data warehouse using the
  [`pull_bio()`](pull_bio.md)

- datTows:

  the catch data frame extracted from the data warehouse using the
  [`pull_catch()`](pull_catch.md)

- strat.vars:

  Variables in both data frame that are used to define the strata.
  Default is bottom depth (m) and latitudes (decimal degrees), i.e.,
  `c("Depth_m", "Latitude_dd")`.

- strat.df:

  A data frame that defines the strata and provides the calculated areas
  for each strata returned from `createStrataDF.fn()`.

- ageBins:

  Vector of age bins to create age compositions across. Values above or
  below the minimum or maximum values, respectively, are grouped into
  the first age or plus group age.

- SSout:

  A logical with the default of `TRUE`. If `TRUE`, the output is
  returned in a format that can be directly pasted into an SS3 data
  file.

- meanRatioMethod:

  A logical with the default of `TRUE`. If `TRUE`, then the mean ratio
  is implemented instead of the total ratio. Search the source code for
  the equations if more information is needed.

- sex:

  Options of (0, 1, 2, 3). The integer will be used to define the sex
  column of the returned input for Stock Synthesis and specifies how the
  composition are treated with respect to sex. See the Stock Synthesis
  manual for more information. In short, 0 is for unsexed, 1 is females,
  2 is males, and 3 is females and males. The default is `3`.

- NAs2zero:

  A logical specifying if `NA`s should be changed to zeros. The default
  is `TRUE`.

- sexRatioUnsexed:

  sex ratio to apply to any length bins of a certain size or smaller as
  defined by the maxSizeUnsexed

- maxSizeUnsexed:

  all sizes below this threshold will assign unsexed fish by sexRatio
  set equal to 0.50, fish larger than this size will have unsexed fish
  assigned by the calculated sex ratio in the data.

- sexRatioStage:

  1/2 apply the sex ratio based on the tows (1) or the expanded numbers
  (2)

- partition:

  Partition to assign the composition data based on the expected format
  for Stock Synthesis. Partition of 0 indicates that the composition
  data include all composition data, 1 for discarded composition data,
  and 2 for retained fish only. Default of 0.

- fleet:

  A single integer value. A user input fleet number to assign to the
  fleet column based on the expected format for Stock Synthesis. Default
  "Enter Fleet".

- agelow:

  Lower age bin for all age composition data based on the expected
  format for Stock Synthesis. Default value of -1 which translates to
  the lowest age bin.

- agehigh:

  Upper age bin for all age composition data based on the expected
  format for Stock Synthesis. Default value of -1 which translates to
  the highest

- ageErr:

  Single integer value of ageing error vector to apply to the age data
  based on Stock Synthesis. Default "Enter".

- nSamps:

  Vector of integer sample sizes. A vector of sample sizes for all years
  in `datA` is required if a vector is provided. The input vector will
  be included in the output marginal age composition data. One option
  for calculating input sample size is the [`GetN.fn()`](GetN.fn.md).
  The default is "Enter Samps".

- month:

  A single integer value between 1-12. A user input fleet number to
  assign to the month column based on the expected format for Stock
  Synthesis. See the Stock Synthesis manual for more information.
  Default "Enter Month".

- printfolder:

  A string that will be appended to `dir`, creating a folder where the
  output will be saved. If specified as `""`, the output will just be
  saved directly in `dir`. The default is `"forSS3"`.

- remove999:

  The output object by the function will have the 999 column combined
  with the first age bin. Default TRUE.

- outputStage1:

  return the first stage expanded data without compiling it for SS3

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Details

Create expanded marginal age composition data based on the pre-specified
strata. This function is designed to be used with catch pulled using
[`pull_catch()`](pull_catch.md) and biological data pulled using
[`pull_bio()`](pull_bio.md). The default output is formatted based on
the formatting required by Stock Synthesis.

## See also

[`SurveyLFs.fn`](SurveyLFs.fn.md)

## Author

Allan Hicks and Chantel Wetzel
