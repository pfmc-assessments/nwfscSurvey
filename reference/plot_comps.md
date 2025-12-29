# Plot frequency data as bubble plots

Plot frequency data as bubble plots

## Usage

``` r
plot_comps(
  data,
  dir = NULL,
  add_save_name = NULL,
  plot = 1:2,
  add_0_ylim = TRUE,
  width = 10,
  height = 7
)
```

## Arguments

- data:

  Data file object created by
  [`get_expanded_comps()`](get_expanded_comps.md) or
  [`get_raw_comps()`](get_raw_comps.md).

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- add_save_name:

  Option to add text to a saved figure name. This option can be useful
  if creating plots across multiple species and saving them into a
  single folder. The default is `NULL`. Note that the data type, i.e.,
  age or length, and sex type are already included in the saved name so
  no need to add those here.

- plot:

  A vector of integers to specify which plots you would like. The
  default is to print or save both figures, i.e., `plot = 1:2`. Integers
  correspond to the following figures:

  1.  bubble plot of length-/age-composition data by year and sex and

  2.  distribution by year of length-/age-compositions data similar to
      the r4ss figure.

- add_0_ylim:

  A logical, i.e., `TRUE`/`FALSE`, argument that specifies if the y axis
  should start at 0. If `FALSE`, the y axis will start at the minimum
  bin size used in data. The default is TRUE. This currently only
  pertains to plot 1, not plot 2.

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 10 by 7 inches.

## Author

Chantel Wetzel

## Examples

``` r
if (FALSE) { # \dontrun{
plot_comps(data = LFs)
} # }
```
