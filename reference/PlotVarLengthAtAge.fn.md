# Plot variability of length at age

Plots the SD and CV of age at observed and predicted length

## Usage

``` r
PlotVarLengthAtAge.fn(
  dir = NULL,
  dat,
  main = NULL,
  ageBin = 1,
  bySex = T,
  parStart = c(52, 0.09, 1),
  estVB = T,
  bins = NULL,
  legX = "bottomleft",
  legY = NULL,
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

- main:

  Name that will be used to name the saved png

- ageBin:

  Currently fixed at 1, so a moot parameter

- bySex:

  Logical to indicate if plot by sex

- parStart:

  Vector of starting parameters for Linf, k, and t0 in VonB estimation

- estVB:

  Logical. Estimate vonB growth to plot against predicted length. If F,
  it uses the parameters in `parStart`.

- bins:

  The bins to put ages into. If NULL then simply uses the ages as
  recorded.

- legX:

  legend location for x axis, defaults to "bottomleft"

- legY:

  legend location for y axis, defaults to NULL

- dopng:

  Deprecated with nwfscSurvey 2.1 because providing a non-NULL value to
  `dir` can serve the same purpose as `dopng = TRUE` without the
  potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
  no longer have to specify `dopng` to save the plot as a png.

- ...:

  Additional arguments for the plots

## Author

Allan Hicks and Chantel Wetzel
