# Expands the lengths up to the total stratum area then sums over strata for each year

Create expanded length composition data based on the pre-specified
strata. This function is designed to be used with catch pulled using
[`pull_catch()`](pull_catch.md) and biological data pulled using
[`pull_bio()`](pull_bio.md). The default output is formatted based on
the formatting required by Stock Synthesis.

## Usage

``` r
SurveyLFs.fn(
  dir = NULL,
  datL,
  datTows,
  strat.vars = c("Depth_m", "Latitude_dd"),
  strat.df = NULL,
  lgthBins = 1,
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
  sum100 = TRUE,
  verbose = TRUE
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- datL:

  A data frame of length-composition data returned from
  [`pull_bio()`](pull_bio.md).

- datTows:

  A data frame of catch data returned from
  [`pull_catch()`](pull_catch.md).

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

  A numerical value within `[0.0, 1.0]` that will be used as the sex
  ratio for measured individuals less than `maxSizeUnsexed`. If
  `NA_real_`, then the sex ratio for stage-1 expansion will not be
  conducted.

- maxSizeUnsexed:

  A numerical value specifying the right side of the following bin
  `[0, maxSizeUnsexed]`, where all fish measured in this bin are
  assigned a sex based on sexRatioUnsexed. Fish with a measurement
  larger than this value will be assigned a sex based on the calculated
  sex ratio in the data.

- sexRatioStage:

  (1, 2). The stage of the expansion to apply the sex ratio. The default
  is `1`.

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
  in `datL` is required if a vector is provided. The input vector will
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
  with the first length bin. Default TRUE.

- outputStage1:

  A logical specifying if you would like the function to stop after the
  end of the first stage of the expansion process and return output that
  is not ready for Stock Synthesis. This can be helpful when wanting
  output that can be used as input for VAST.

- sum100:

  A logical value specifying whether to rescale the compositions to sum
  to 100. The default is `TRUE`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## See also

- [`StrataFactors.fn()`](StrataFactors.fn.md)

- [`SexRatio.fn()`](SexRatio.fn.md)

## Author

Allan Hicks (16 March 2009) and Chantel Wetzel (maintainer)
