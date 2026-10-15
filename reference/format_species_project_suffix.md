# Format Species and Project Values for Filenames

Build a filename suffix from the first values of the `common_name` and
`project` columns. Values are converted to lowercase and spaces are
replaced with underscores. Missing columns and empty values are omitted.

## Usage

``` r
format_species_project_suffix(data)
```

## Arguments

- data:

  A data frame that may contain `common_name` and `project` columns.

## Value

A character string beginning with an underscore when at least one
non-empty value is present, or an empty string otherwise.
