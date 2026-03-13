# Create strata data frame using [`get_strata_areas`](get_strata_areas.md)

Create strata data frame using [`get_strata_areas`](get_strata_areas.md)

## Usage

``` r
create_strata(names = NA, depths_shallow, depths_deep, lats_south, lats_north)
```

## Arguments

- names:

  A vector of character values providing a name for each strata; if left
  at the default value of `NA`, then names will be created using capital
  letters starting with `A` for each `NA` value.

- depths_shallow:

  vector of the shallow depths splits for each strata

- depths_deep:

  vector of the deep depths splits for each strata

- lats_south:

  vector of the southern latitude splits for each strata

- lats_north:

  vector of the northern latitude splits for each strata

## Value

Returns the data frame formatted for use by
[`get_design_based`](get_design_based.md) and additional prediction
functions in other downstream packages. The data frame will have six
columns, (1) name, (2) area, (3) Depth_m.1, (4) Depth_m.2, (5)
Latitude_dd.1, and (6) Latitude_dd.2.

## Details

Create a data frame of strata formatted for use by models that need
estimates of the area for each strata to expand estimates of abundance.
Strata limits are provided by the user in terms of and east and west
ranges using depth (shallow and deep, respectively) and north and south
ranges using latitudes.

## See also

See [`get_strata_areas`](get_strata_areas.md) for how areas are
calculated. See [`get_design_based`](get_design_based.md) for how the
areas are used to create design-based biomass estimates.

Other strata function: [`CreateStrataDF.fn()`](CreateStrataDF.fn.md)

## Author

Chantel Wetzel and Kelli Johnson

## Examples

``` r
strata <- create_strata(
  names          = c("deep_south", "shallow_south", "deep_central_north", "shallow_central", "shallow_north"),
  depths_shallow = c(183, 55, 183, 55, 55),
  depths_deep    = c(549, 183, 549, 183, 183),
  lats_south     = c(32, 32, 40, 40, 44),
  lats_north     = c(40, 40, 49, 44, 49)
)
```
