# Function to plot sex ratio

Function to plot sex ratio

## Usage

``` r
PlotSexRatio.fn(
  dir,
  dat,
  data.type = "length",
  main = NULL,
  circleSize = 0.1,
  dopng = lifecycle::deprecated(),
  ...
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- dat:

  Data object with biological data from [`pull_bio()`](pull_bio.md) with
  a column named `Sex` present.

- data.type:

  Specify where to calculate the sex ration by length or age.

- main:

  Name that will be used to name the saved png

- circleSize:

  circle size

- dopng:

  Deprecated with nwfscSurvey 2.1 because providing a non-NULL value to
  `dir` can serve the same purpose as `dopng = TRUE` without the
  potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
  no longer have to specify `dopng` to save the plot as a png.

- ...:

  Additional arguments for the plots

## Author

Allan Hicks and Chantel Wetzel
