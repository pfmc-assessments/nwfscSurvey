# Plot length or age compositions by year in bubble plots

Plot length or age compositions by year in bubble plots

## Usage

``` r
PlotFreqData.fn(
  dir = NULL,
  dat,
  inch = 0.15,
  ylab = "Bins",
  xlab = "Year",
  zero2NAs = TRUE,
  main = NULL,
  xlim = NULL,
  ymax = NULL,
  dopng = lifecycle::deprecated(),
  w = 7,
  h = 7,
  ...
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- dat:

  Object created by [`SurveyLFs.fn()`](SurveyLFs.fn.md) or
  [`SurveyAFs.fn()`](SurveyAFs.fn.md)

- inch:

  input to the symbols plot: TRUE, FALSE or a positive number.

- ylab:

  y-axis text label

- xlab:

  x-axis text label

- zero2NAs:

  A logical specifying if `NA`s should be changed to zeros. The default
  is `TRUE`.

- main:

  main plot text

- xlim:

  x-limit values

- ymax:

  Value used to truncate y-axis, defaults to NULL

- dopng:

  Deprecated with nwfscSurvey 2.1 because providing a non-NULL value to
  `dir` can serve the same purpose as `dopng = TRUE` without the
  potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
  no longer have to specify `dopng` to save the plot as a png.

- w:

  Numeric figure width, defaults to 7

- h:

  Numeric figure height, defaults to 7

- ...:

  Additional arguments for the plots

## Author

Chantel Wetzel and Allan Hicks
