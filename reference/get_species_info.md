# Get Species Information

Get Species Information

## Usage

``` r
get_species_info(species, unident = FALSE, verbose = TRUE)
```

## Arguments

- species:

  A vector of species names. The words can be separated using spaces or
  underscores. Full names are not required but they will increase the
  consistency of the results should partial matches return multiple
  matches.

- unident:

  A logical entry with the default value of `FALSE`, to match historical
  output that did not include unidentified groups.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

A data frame with the scientific name in the latin column and in the
scientific_name column that has spaces replaced with underscores; common
name in the common column and in the common_name column that has spaces
replaced with underscores; species values used as input in the input
column; and strata used to assess its status in the strata column.

## Details

Get the scientific name, common name, and strata group for a vector of
species.

## See also

See [`PullSpp.fn`](PullSpp.fn.md) for information on common and
scientific names; and [`GetStrata.fn`](GetStrata.fn.md) for the
different stratifications.

## Author

Kelli Faye Johnson

## Examples

``` r
get_species_info(c("sablefish", "petrale"))
#> Error in if (lapply(index, length) > 1) {    cli::cli_alert_warning("Multiple matches were found for the {species} in the look up table\n           stored in pull_spp(). Only one match is returned.")    index <- index[[1]][1]}: the condition has length > 1
get_species_info(c("vermilion"))
#> ! Multiple matches were found for the vermilion in the look up table
#> stored in pull_spp(). Only one match is returned.
#>                 latin             common        common_name   scientific_name
#> 644 Sebastes miniatus vermilion rockfish vermilion_rockfish Sebastes_miniatus
#>         input strata species_type
#> 644 vermilion  coast    shelfrock
testthat::expect_message(
  get_species_info(c("chilipepper"))
)
```
