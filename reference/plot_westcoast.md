# Plot the United States west coast using ggplot2

Plot the United States west coast such that it can used as a standalone
figure or as a base to plot data points on.

## Usage

``` r
plot_westcoast()
```

## Value

A ggplot2 object.

## See also

- [`PlotMap.fn()`](PlotMap.fn.md) uses this function as the base map.

## Author

Kelli F. Johnson

## Examples

``` r
if (FALSE) { # \dontrun{
if (require("maps") && require("mapproj")) {
  map_object <- plot_westcoast()
  print(map_object)
  ggplot2::ggsave(
    filename = "myfilename.png",
    plot = map_object
  )
}
} # }
```
