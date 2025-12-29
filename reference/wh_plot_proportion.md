# Save figures of proportions by depth and latitude using warehouse data

Four figures in total are created and saved to the disk if both catch
and biological data, pulled from the data warehouse, are passed to
`data_catch` and `data_bio`, respectively. This function will only work
with data that has the standard column names of data pulled from the
warehouse.

## Usage

``` r
wh_plot_proportion(
  data_catch,
  data_bio,
  dir = file.path(getwd(), "plots"),
  bar_width = c("n", "equal")
)
```

## Arguments

- data_catch, data_bio:

  Data frames returned from [`pull_catch()`](pull_catch.md) and
  \[pull_bio())\], respectively. At least one of the arguments must be
  passed.

  \[pull_bio())\]: R:pull_bio())

- dir:

  The directory where you would like the `.png` files to be saved. The
  default is a directory called `"plots"` in your current working
  directory.

- bar_width:

  A string of `"n"` or `"equal"`, where the former leads to bar widths
  based on the number of observations contained in that group and the
  latter leads to equally-sized bars for all bars with data. For groups
  without any data, the width of the placeholder on the x axis will be
  smaller than the width of the bars for groups with data regardless of
  which option you choose. The default is to have bars of variable
  widths, i.e., `"n"`.

## Value

Strings of the saved files.

## See also

- [`plot_proportion()`](plot_proportion.md)

- [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html)

## Author

Chantel R. Wetzel and Kelli F. Johnson

## Examples

``` r
if (FALSE) { # \dontrun{
test <- wh_plot_proportion(catch_nwfsc_combo, bio_nwfsc_combo)
} # }
```
