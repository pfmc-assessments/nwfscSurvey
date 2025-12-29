# Calculate unexpanded/raw length or marginal age compositions

Calculate unexpanded/raw length or marginal age compositions

## Usage

``` r
get_raw_caal(
  data,
  len_bins,
  age_bins,
  length_column_name = "length_cm",
  age_column_name = "age",
  dir = NULL,
  month = "Enter Month",
  fleet = "Enter Fleet",
  partition = 0,
  ageerr = "Enter Numeric",
  printfolder = "forSS3",
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame that includes columns of year, sex, and length/ages. The
  data frame can be survey data pulled using pull_bio from the data
  warehouse or any data frame that includes column names of sex, year,
  and the comp_column_name. The sex column is expected to have sexes
  denoted by F, M, and U.

- len_bins:

  Vector of integers to bin length data by create expanded composition
  data. Values above or below the minimum or maximum values in the
  vector are grouped into the first size or plus group size,
  respectively. For example, creating length compositions that uses a
  vector bin of seq(10, 50, 2) would create 2 cm bins where fish length
  between \[0, 11.99) would be included in the 10 cm bin, fish of length
  \[12, 13.99) would be included in the 12 cm bin, and all fish \[50-
  Inf) would be included in the 50 cm plus bin.

- age_bins:

  Vector of integers to bin age data by create expanded composition
  data. Values above or below the minimum or maximum values in the
  vector are grouped into the first size or plus group size,
  respectively. For example, creating age compositions that uses a
  vector bin of seq(1, 50, 1) would create 1 year age bins and all ages
  \[50-Inf) will be in the plus group age bin.

- length_column_name:

  The length column name to create conditional age-at-length data for.
  The default is `length_cm`.

- age_column_name:

  The age column name to create conditional age-at-length data for. The
  default is `age`.

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- month:

  Month the samples were collected based on the expected format for
  Stock Synthesis to determine the length/age estimate to compare to.
  Default "Enter Month".

- fleet:

  A fleet number to assign the composition data to based on the expected
  format for Stock Synthesis. Default "Enter Fleet".

- partition:

  Partition to assign the composition data based on the expected format
  for Stock Synthesis. Partition of 0 indicates that the composition
  data include all composition data, 1 for discarded composition data,
  and 2 for retained fish only. Default of 0.

- ageerr:

  Number of ageing error vector to apply to the age data based on Stock
  Synthesis. Default "Enter Numeric".

- printfolder:

  A string that will be appended to `dir`, creating a folder where the
  output will be saved. If specified as `""`, the output will just be
  saved directly in `dir`. The default is `"forSS3"`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

A data frame of conditional age-at-length compositions for sexed and
unsexed fish formatted for Stock Synthesis.

## Details

Creates a matrix of unexpanded (or raw) marginal length or age
composition data formatted for Stock Synthesis. The code will return
composition data for all sexes present in the data frame and no sex
assignment is done for unsexed fish. The function will create
composition data for either lengths or ages based on the
comp_column_name. The function will return a list of composition data
based upon the sexes present in the data for a two-sex model or all
length/ages for single-sex model.

## Author

Chantel Wetzel

## Examples

``` r
if (FALSE) { # \dontrun{
bio <- pull_bio(
  common_name = "lingcod",
  survey = "NWFSC.Combo"
)

caal_data <- get_raw_caal(
  data = bio,
  len_bins = seq(20, 70, 4),
  age_bins = 1:30,
)
} # }
```
