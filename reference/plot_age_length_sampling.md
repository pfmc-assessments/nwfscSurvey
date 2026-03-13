# Plot the representativeness of age sampling based on lengths

Plot the representativeness of age sampling based on lengths

## Usage

``` r
plot_age_length_sampling(
  data,
  dir = NULL,
  height = 7,
  width = 7,
  xlim = lifecycle::deprecated(),
  ylim = lifecycle::deprecated()
)
```

## Arguments

- data:

  data frame

- dir:

  Defaults to NULL (plot made, but not saved to file). Can alternatively
  be a string filename by year and sex

- width, height:

  Numeric values for the figure width and height in inches. The defaults
  are 7 by 7 inches.

- xlim:

  Deprecated with nwfscSurvey version 2.8.0.1 because the limits are now
  automatically determined. x limits for plot, defaults to c(0,120)

- ylim:

  Deprecated with nwfscSurvey version 2.8.0.1 because the limits are now
  automatically determined. y limits for plot, defaults to (0, 0.049)

## Author

Chantel Wetzel
