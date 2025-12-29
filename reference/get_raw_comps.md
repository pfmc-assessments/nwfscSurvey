# Calculate unexpanded/raw length or marginal age compositions

Calculate unexpanded/raw length or marginal age compositions

## Usage

``` r
get_raw_comps(
  data,
  comp_bins,
  comp_column_name = "Length_cm",
  input_n_method = c("stewart_hamel", "tows", "total_samples"),
  two_sex_comps = TRUE,
  month = "Enter Month",
  fleet = "Enter Fleet",
  partition = 0,
  ageerr = "Enter Numeric",
  Lbin_lo = -1,
  Lbin_hi = -1,
  age_low = lifecycle::deprecated(),
  age_high = lifecycle::deprecated(),
  age_error = lifecycle::deprecated(),
  dir = NULL,
  printfolder = "forSS3",
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame that includes columns of year, sex, and length/ages. The
  case of the column names does not matter as the function reduces them
  to lower case. The data frame can be survey data pulled using pull_bio
  from the data warehouse or any data frame that includes column names
  of sex, year, and the comp_column_name. The sex column is expected to
  have sexes denoted by F, M, and U. If tows/trips are desired for
  `input_n_method` and a column exists within the data frame with this
  information, the column needs to be named `trawl_id`, otherwise the
  function will set a unique `trawl_id` for each record.

- comp_bins:

  Vector of integers to bin length or age data by create expanded
  composition data.Values above or below the minimum or maximum values
  in the vector are grouped into the first size or plus group size,
  respectively. For example, creating length compositions that uses a
  vector bin of seq(10, 50, 2) would create 2 cm bins where fish length
  between \[0, 11.99) would be included in the 10 cm bin, fish of length
  \[12, 13.99) would be included in the 12 cm bin, and all fish \[50-
  Inf) would be included in the 50 cm plus bin.

- comp_column_name:

  The column name to create composition data for. This column can be is
  used to determine whether to format the composition data for length or
  age compositions by looking for either age (e.g., `age_years`, `Age`,
  `best_age`) or length (e.g., `Length`, `length`, `Length_cm`) in the
  comp_column_name. The default is `Length_cm`.

- input_n_method:

  Determines the default input sample size to add to the composition
  data for SS3. There are three options: c("stewart_hamel", "tows",
  "total_samples") where the default is "stewart_hamel".

- two_sex_comps:

  Default TRUE. If TRUE composition data will be formatted for a Stock
  Synthesis two-sex model and if FALSE composition data will be
  formatted for a single-sex model.

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

- Lbin_lo:

  Lower age bin for all age composition data based on the expected
  format for Stock Synthesis. Default value of -1 which translates to
  the lowest age bin.

- Lbin_hi:

  Upper age bin for all age composition data based on the expected
  format for Stock Synthesis. Default value of -1 which translates to
  the highest

- age_low:

  Deprecated with nwfscSurvey 2.2. Use Lbin_lo instead.

- age_high:

  Deprecated with nwfscSurvey 2.2. Use Lbin_hi instead.

- age_error:

  Deprecated with nwfscSurvey 2.2. Use Lbin_hi instead.

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- printfolder:

  A string that will be appended to `dir`, creating a folder where the
  output will be saved. If specified as `""`, the output will just be
  saved directly in `dir`. The default is `"forSS3"`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

A list of length or marginal age compositions for sexed and unsexed fish
formatted for Stock Synthesis.

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

length_comps <- get_raw_comps(
  data = bio,
  comp_bins = seq(20, 70, 4)
)

age_comps <- get_raw_comps(
  data = bio,
  comp_bins = 1:20,
  comp_column_name = "Age"
)
} # }
```
