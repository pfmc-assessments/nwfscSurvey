# Get Default Strata

Get Default Strata

## Usage

``` r
GetStrata.fn(area = "coast")
```

## Arguments

- area:

  A single character value of the area or areas you want to predict to.
  Multiple areas must be separated with an underscore, e.g., `"wa_or"`
  or `"north_south"`. The default is `"coast"`, which provides shallow
  and deep strata for each state; thereby, covering the entire US West
  Coast.

## Value

A data frame with six columns,

- name,

- area,

- Depth_m.1 (m; the shallow depth border),

- Depth_m.2 (m; the deep depth border),

- Latitude_dd.1 (decimal degrees; southern border), and

- Latitude_dd.2 (decimal degrees; northern border).

## Details

Get a data frame of strata for design-based estimates and survey
composition data. Several strata are available for different default
areas, as well as combinations using `"_"` in the argument `area`.

## See also

This function uses [`CreateStrataDF.fn`](CreateStrataDF.fn.md) to
generate the strata returned by this function. Also, see
convert_strata4vast function in the VASTWestCoast package that converts
results from this function to strata that can be used within the VAST
package.

## Author

Chantel Wetzel and Kelli Faye Johnson
