# Creates a vector of strata factors by request column names

Creates a vector of strata factors by request column names

## Usage

``` r
strata_factors(data, strata_df, strata_vars = c("Latitude_dd", "Depth_m"))
```

## Arguments

- data:

  Catch dataframe created by \[pull_catch()\]

- strata_df:

  Strata dataframe created by [`create_strata()`](create_strata.md)

- strata_vars:

  Column names in `data` to define the stratas by. The default columns
  are `Depth_m` or `depth_m` and `Latitude_dd` or `latitude_dd`.

## Author

Chantel Wetzel
