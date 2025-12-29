# Utility function used throughout the package

Utility function used throughout the package

## Usage

``` r
convert_to_hex_string(x)
```

## Arguments

- x:

  A string of either common_name or sci_name

## Details

Function that converts a string to a hex string for common name or
scientific name when pulling data. This function is used within the
pull\_\* functions that retrieve species specific data

## Author

Kelli Johnson

## Examples

``` r
if (FALSE) { # \dontrun{
common_name <- c("lingcod", "sablefish", "Pacific cod")
convert_to_hex_string(common_name)
} # }
```
