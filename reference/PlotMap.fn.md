# Plot two figures showing catch-per-unit-effort data

Plot catch-per-unit-effort (CPUE) data across all years and by year and
save them to the disk. Figures are created using ggplot2.

## Usage

``` r
PlotMap.fn(
  dir = NULL,
  dat,
  main = NULL,
  dopng = lifecycle::deprecated(),
  plot = 1:2
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- dat:

  An object created by [`pull_catch()`](pull_catch.md).

- main:

  A string that will be prepended to the name of the saved png (i.e.,
  "NWFSC" results in a file called "NWFSC_CPUE_Map.png").

- dopng:

  Deprecated with nwfscSurvey 2.1 because providing a non-NULL value to
  `dir` can serve the same purpose as `dopng = TRUE` without the
  potential for errors when `dopng = TRUE` and `dir = NULL`. Thus, users
  no longer have to specify `dopng` to save the plot as a png.

- plot:

  A vector of integers to specify which plots you would like. The
  default is to print both figures.

  1.  coastwide data across all years

  2.  coastwide data by year

## Value

Figures are saved to the disk according to which plots are asked for in
`plot`. Each of the specified files are saved to a directory called
`map_plots` inside of `dir`, the specified directory. No objects are
returned to the user. But, the figures are printed to new windows if
they are not saved to the disk.

## See also

- [`PullCatch.fn()`](PullCatch.fn.md)

- [`plot_westcoast()`](plot_westcoast.md)

## Author

Chantel R. Wetzel

## Examples

``` r
if (FALSE) { # \dontrun{
PlotMap.fn(dat = catch_nwfsc_combo, plot = 1)
PlotMap.fn(dat = catch_nwfsc_combo, plot = 2)
} # }
```
