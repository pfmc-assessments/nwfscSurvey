# Format NWFSC Hook-and-Line biological data

This function will filter biological data to only include observations
for the `common_name`. The returned data frame will include length and
age data for the selected species along with all the other hook-and-line
survey information.

## Usage

``` r
format_hkl_bio_data(data, common_name = NULL, verbose = TRUE)
```

## Arguments

- data:

  Data frame of NWFSC Hook-and-Line data.

- common_name:

  Species names as given in the hook and line data set to format data
  for.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

Returns a data frame of formatted hook-and-line biological data

## Author

Chantel Wetzel
