# Function to create url to pull data from the data warehouse

Function to create url to pull data from the data warehouse

## Usage

``` r
get_url(data_table, project_long, add_species, years, vars_long)
```

## Arguments

- data_table:

  table to pull from the data warehouse, options = trawl.catch_fact,
  trawl.operation_haul_fact

- project_long:

  survey project name

- add_species:

  string of species name created by the pull_catch or pull_bio
  functions.

- years:

  An integer vector of length two with the range of years to pull data
  for (e.g., c(2003, 2024)). Vector can not contain -Inf or Inf.

- vars_long:

  string of fields to pull from the data warehouse

## Author

Chantel Wetzel
