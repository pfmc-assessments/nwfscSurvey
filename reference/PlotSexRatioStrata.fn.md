# Function to plot sex ratio by strata

Function to plot sex ratio by strata

## Usage

``` r
PlotSexRatioStrata.fn(
  dir = NULL,
  dat,
  type = "length",
  strat.vars = c("Depth_m", "Latitude_dd"),
  strat.df = NULL,
  circleSize = 0.05,
  dopng = lifecycle::deprecated(),
  ...
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- dat:

  A data frame of length-composition data returned from
  [`pull_bio()`](pull_bio.md).

- type:

  Specify where to calculate the sex ration by length or age.

- strat.vars:

  Variables in both data frame that are used to define the strata.
  Default is bottom depth (m) and latitudes (decimal degrees), i.e.,
  `c("Depth_m", "Latitude_dd")`.

- strat.df:

  A data frame that defines the strata and provides the calculated areas
  for each strata returned from `createStrataDF.fn()`.

- circleSize:

  circle size

- dopng:

  Deprecated with nwfscSurvey 2.1 because providing a non-NULL value to
  `dir` can serve the same purpose as `dopng = TRUE` without the
  potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
  no longer have to specify `dopng` to save the plot as a png.

- ...:

  Additional arguments for the plots

## See also

[`StrataFactors.fn`](StrataFactors.fn.md)

## Author

Allan Hicks and Chantel Wetzel
