# Plot proportions of factor levels (y axis) by bins (x axis)

Bin a numerical column for the categorical x axis. Calculate the
proportion of the rows that are present for each level of an additional
factor column, like sex, by x-axis bins.

## Usage

``` r
plot_proportion(
  data,
  column_factor,
  column_bin,
  digits = 0,
  bar_width = c("n", "equal"),
  ...
)
```

## Arguments

- data:

  A data frame.

- column_factor:

  \<`data-masking`\> Variable in `data` for the grouping structure.
  Should be a variable in `data` that contains a factor but it can also
  be a variable in `data` that stores characters. See the examples for
  ideas.

- column_bin:

  \<`data-masking`\> Variable in `data` for the binning structure. If
  this is not already a factor or character, then
  [`ggplot2::cut_width()`](https://ggplot2.tidyverse.org/reference/cut_interval.html)
  will be used to create bins. The bins are saved in `bin` in the
  returned ggplot2 object.

- digits:

  A numeric value passed to the digits argument of
  [`round()`](https://rdrr.io/r/base/Round.html). Positive values
  pertain to rounding the right side of the decimal place and negative
  values pertain to rounding the left side of the decimal place, i.e.,
  to the tens or hundreds with -1 and -2. The default is to round to the
  nearest integer using `digits = 0`.

- bar_width:

  A string of `"n"` or `"equal"`, where the former leads to bar widths
  based on the number of observations contained in that group and the
  latter leads to equally-sized bars for all bars with data. For groups
  without any data, the width of the placeholder on the x axis will be
  smaller than the width of the bars for groups with data regardless of
  which option you choose. The default is to have bars of variable
  widths, i.e., `"n"`.

- ...:

  Additional arguments that users want to pass to
  [`ggplot2::cut_width()`](https://ggplot2.tidyverse.org/reference/cut_interval.html).
  If `data[[column_bin]]` is not a factor, then at least `width` needs
  to be passed to create bins. But, any argument accepted by
  [`ggplot2::cut_width()`](https://ggplot2.tidyverse.org/reference/cut_interval.html)
  can be passed, where `boundary = 0` is common so the calculation of
  bins will start at zero rather than the minimum value present,
  preventing bins like (35.4, 36.4\] when you really want (35, 36\].

## Value

A ggplot2 object created with ggmosaic features.

## Details

[ggmosaic](https://haleyjeppson.github.io/ggmosaic/) is used to create a
ggplot2 object for categorical data that can be modified upon return.

## See also

- [`ggplot2::cut_width()`](https://ggplot2.tidyverse.org/reference/cut_interval.html)

- [`ggmosaic::geom_mosaic()`](https://haleyjeppson.github.io/ggmosaic/reference/geom_mosaic.html)

- [`factor()`](https://rdrr.io/r/base/factor.html)

## Author

Ian G. Taylor, Chantel R. Wetzel, Kelli F. Johnson

## Examples

``` r
if (FALSE) { # \dontrun{
# Add presence/absence factor to data
#temp <- catch_nwfsc_combo |>
#   dplyr::mutate(new = factor(
#    cpue_kg_km2 <= 0,
#     levels = c(FALSE, TRUE),
#     labels = c("Present", "Absent")
#   ))

# Plot depth bins (50 m) by presence/absence with default colors
#plot_proportion(
#  data = temp,
#  column_factor = new,
#  column_bin = Depth_m,
#  width = 50,
#  boundary = 0
#)
# Plot latitude bins (1 decimal degree) by presence/absence with custom
# colors
#plot_proportion(
#  data = temp,
#  column_factor = new,
#  column_bin = Latitude_dd,
#  width = 1,
#  boundary = 0
#) +
#  ggplot2::scale_fill_manual(values = c(
#    "darkorchid3",
#    grDevices::gray(0.7)
#  ))
# Plot depth bins (25 m) by sex (F, M, U)
#plot_proportion(
#  data = bio_nwfsc_combo |>
#    dplyr::mutate(Sex = codify_sex(Sex)),
#  column_factor = Sex,
#  column_bin = Depth_m,
#  width = 25,
#  boundary = 0
#)
# Change to equal sized bars
#plot_proportion(
#  data = bio_nwfsc_combo |>
#    dplyr::mutate(Sex = codify_sex(Sex)),
#  column_factor = Sex,
#  column_bin = Depth_m,
#  width = 25,
#  boundary = 0,
#  bar_width = "equal"
#)
} # }
```
