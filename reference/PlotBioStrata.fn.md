# Plots the design-based biomass estimates by strata with confidence intervals

Plots the design-based biomass estimates by strata with confidence
intervals

## Usage

``` r
PlotBioStrata.fn(
  dir = NULL,
  dat,
  CI = 0.95,
  scalar = 1e+06,
  gap = 0.03,
  ylab = "Biomass ('000 mt)",
  xlab = "Year",
  survey.name = NULL,
  strata.names = NULL,
  ylim = NULL,
  sameylim = FALSE,
  add = FALSE,
  mfrow.in = NULL,
  col = "black",
  pch.col = "black",
  pch.type = 16,
  dopng = lifecycle::deprecated(),
  ...
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- dat:

  Data frame created by the [`Biomass.fn()`](Biomass.fn.md)

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

- survey.name:

  if specified will name the output png file using this name

- strata.names:

  custom strata names, if not specified will use the defined names from
  [`CreateStrataDF.fn()`](CreateStrataDF.fn.md)

- ylim:

  y-limits

- sameylim:

  Boolean, whether to include same y-limits or not. Defaults to FALSE

- add:

  add additional line to plot

- mfrow.in:

  option to specify the mfrow for plotting

- col:

  color

- pch.col:

  Color as string, defaults to "black"

- pch.type:

  Numeric pch type, defaults to 16

- dopng:

  Deprecated with nwfscSurvey 2.1 because providing a non-NULL value to
  `dir` can serve the same purpose as `dopng = TRUE` without the
  potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
  no longer have to specify `dopng` to save the plot as a png.

- ...:

  Additional arguments for the plots

## Author

Chantel Wetzel, Allan Hicks, and John Wallace
