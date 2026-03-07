# This function plots cpue and length by latitude and depth

This function plots cpue and length by latitude and depth

## Usage

``` r
plot_cpue(catch, dir = NULL, plot = 1:3, width = 7, height = 7, ...)
```

## Arguments

- catch:

  Data catch file pulled using [`pull_catch()`](pull_catch.md)

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- plot:

  A vector of integers to specify which plots to return. The default is
  to print or save all figures, i.e., `plot = 1:3`. Integers correspond
  to the following figures:

  1.  log(CPUE) by depth & log(CPUE) by latitude

  2.  log(CPUE) by latitude and year

  3.  log(CPUE) by depth and year

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 7 by 7 inches.

- ...:

  Additional arguments to `ggsave()`

## Author

Chantel Wetzel
