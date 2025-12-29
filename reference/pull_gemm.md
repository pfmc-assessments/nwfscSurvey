# Pull Groundfish Expanded Multiyear Mortality data

The data are being pulled from:
https://connect.fisheries.noaa.gov/gemm_csv/ This function can be used
to pull all gemm data, a single species, or a subset of species
c("Canary Rockfish", "Widow Rockfish"). Species names in the gemm are
capitalized (e.g. Canary Rockfish). However, there are checks in the
function to adjust input species names if the input names do not match
the expected capitalization (e.g. "canary rockfish", "canary_rockfish").
The function also allows you to subset the data by year using the years
input and to save the object if the dir function input is given.

## Usage

``` r
pull_gemm(common_name, years, dir = NULL, verbose = TRUE)
```

## Arguments

- common_name:

  A character entry with the desired common name of the species you want
  to pull data for from the data warehouse. Use a vector of names if you
  want information for more than one species or if the desired species
  is included in the database using more than one name, e.g., vermilion
  rockfish (see the example below). Use the `sci_name` argument if you
  know the latin name.

- years:

  An integer vector of length two with the range of years to pull data
  for (e.g., c(2003, 2024)). Vector can not contain -Inf or Inf.

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Author

Chantel Wetzel

## Examples

``` r
if (FALSE) { # \dontrun{
# Pull all GEMM data
all_data <- pull_gemm()

# Pull for a specific specis
widow_data <- pull_gemm(common_name = "widow rockfish")

# Pull multiple species
data <- pull_gemm(common_name = c("Widow Rockfish", "Canary Rockfish"))

# Pull species and subset years
widow_recent <- pull_gemm(common_name = "Widow Rockfish", years = 2014:2019)
} # }
```
