# Sql pull of species names

A static version of the sql call to pull species taxonomic information
from the data warehouse.

Pull common name and scientific name information from the data
warehouse.

## Usage

``` r
pull_spp

pull_spp()
```

## Format

A data frame with four columns, latin, common, common_name, and
scientific_name.

## See also

`pull_spp` for more information.

## Author

Curt Whitmire

Kelli Faye Johnson

## Examples

``` r
if (FALSE) { # \dontrun{
spp <- pull_spp()
} # }
```
