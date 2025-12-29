# Plots the designed based biomass estimates with confidence intervals

Plots the designed based biomass estimates with confidence intervals

## Usage

``` r
PlotBio.fn(
  dir = NULL,
  dat,
  CI = 0.95,
  scalar = 1e+06,
  gap = 0.03,
  ylab = "Biomass ('000 mt)",
  xlab = "Year",
  main = NULL,
  ylim = NULL,
  add = FALSE,
  col = "black",
  dopng = lifecycle::deprecated(),
  ...
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- dat:

  object created by the `Biomasss.fn()`

- CI:

  A numerical value that specifies the confidence interval to return.
  Values should be between 0.01 to 0.99.

- scalar:

  simply the divisor for the biomass

- gap:

  a value that introduces a slight gap between the point estimate and
  the start of the line for the CI. A gap too large will invert the CI,
  making it look huge. You should know when this happens

- ylab:

  y-axis text label

- xlab:

  x-axis text label

- main:

  plot label

- ylim:

  y-limits

- add:

  add additional line to plot

- col:

  color

- dopng:

  Deprecated with nwfscSurvey 2.1 because providing a non-NULL value to
  `dir` can serve the same purpose as `dopng = TRUE` without the
  potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
  no longer have to specify `dopng` to save the plot as a png.

- ...:

  Additional arguments for the plots

## Author

Chantel Wetzel, Allan Hicks, and John Wallace
