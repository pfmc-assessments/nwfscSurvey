# Calculate expanded composition data for lengths and marginal ages

Conduct a two-stage expansion of length and age composition data. The
first stage expands data up to the tow level and the second stage
expands the data up the user-defined strata areas. This function is
designed to be used with catch pulled using
[`pull_catch()`](pull_catch.md) and biological data pulled using
[`pull_bio()`](pull_bio.md).

## Usage

``` r
get_expanded_comps(
  bio_data,
  catch_data,
  comp_bins,
  strata,
  dir = NULL,
  comp_column_name = "length_cm",
  output = c("full_expansion_ss3_format", "tow_expansion_only",
    "full_expansion_unformatted"),
  two_sex_comps = TRUE,
  input_n_method = c("stewart_hamel", "tows", "total_samples"),
  month = "Enter Month",
  fleet = "Enter Fleet",
  partition = 0,
  ageerr = "Enter Numeric",
  Lbin_lo = -1,
  Lbin_hi = -1,
  age_low = lifecycle::deprecated(),
  age_high = lifecycle::deprecated(),
  age_error = lifecycle::deprecated(),
  printfolder = "forSS3",
  verbose = TRUE
)
```

## Arguments

- bio_data:

  A data frame of length-composition data returned from
  [`pull_bio()`](pull_bio.md).

- catch_data:

  A data frame of catch data returned from
  [`pull_catch()`](pull_catch.md).

- comp_bins:

  Vector of integers to bin length or age data by create expanded
  composition data.Values above or below the minimum or maximum values
  in the vector are grouped into the first size or plus group size,
  respectively. For example, creating length compositions that uses a
  vector bin of seq(10, 50, 2) would create 2 cm bins where fish length
  between \[0, 11.99) would be included in the 10 cm bin, fish of length
  \[12, 13.99) would be included in the 12 cm bin, and all fish \[50-
  Inf) would be included in the 50 cm plus bin.

- strata:

  A data frame that defines the strata and provides the calculated areas
  for each strata returned from `createStrataDF.fn()`.

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- comp_column_name:

  The name of the column to create composition data for that must be a
  string. This column can be is used to determine whether to format the
  composition data for length or age compositions by looking for either
  age (e.g., `age_years`, `Age`, `age`, `best_age`) or length (e.g.,
  `length`, `length_cm`, `Length`, `Length_cm`) in the comp_column_name.
  The comp_column_name is not case sensitive.The default is `length_cm`.

- output:

  Switch to specify how to return the composition data where the options
  are c("full_expansion_ss3_format", "tow_expansion_only",
  "full_expansion_unformatted"). The default is
  `output = "full_expansion_ss3_format"` where a list is returned with
  formatted composition data for SS3. The `tow_expansion_only` returns a
  dataframe of composition data only expanded up to the tow level
  (first-stage) and `full_expansion_unformatted` returns a dataframe
  with compositon data expanded up to the tow and strata level but not
  formatted for SS3.

- two_sex_comps:

  Default TRUE. If TRUE composition data will be formatted for a Stock
  Synthesis two-sex model and if FALSE composition data will be
  formatted for a single-sex model.

- input_n_method:

  Determines the default input sample size to add to the composition
  data for SS3. There are three options: c("stewart_hamel", "tows",
  "total_samples") where the default is "stewart_hamel".

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

- printfolder:

  A string that will be appended to `dir`, creating a folder where the
  output will be saved. If specified as `""`, the output will just be
  saved directly in `dir`. The default is `"forSS3"`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

A list or dataframe is returned depending upon `output`. The default
`output = "full_expansion_ss3_format"` returns a list of expanded
composition data by sex grouping (e.g., sexed and unsexed fish)
formatted for Stock Synthesis v.3.30+. If
`output = "full_expansion_unformatted"` a dataframe is returned of
unformatted expanded composition data and if
`output = "tow_expansion_only"` a dataframe is returned with the
composition data only expanded to the tow level (first stage expansion
only).

## See also

See [`get_input_n`](get_input_n.md) for information on input sample size
calculations.

## Author

Chantel Wetzel and Allan Hicks

## Examples

``` r
if (FALSE) { # \dontrun{
bio <- pull_bio(
  common_name = "lingcod",
  survey = "NWFSC.Combo"
)

catch <- pull_catch(
  common_name = "lingcod",
  survey = "NWFSC.Combo"
)

strata <- CreateStrataDF.fn(
  names = c("shallow_wa", "shallow_or", "shallow_nca", "shelf_wa", "shelf_or", "shelf_nca"),
  depths.shallow = c(55, 55, 55, 183, 183, 183),
  depths.deep = c(183, 183, 183, 350, 350, 350),
  lats.south = c(46.0, 42.0, 40.10, 46.0, 42.0, 40.10),
  lats.north = c(49.0, 46.0, 42.0, 49.0, 46.0, 42.0)
)

length_comps <- get_expanded_comps(
  bio_data = bio,
  catch_data = catch,
  strata = strata,
  comp_bins = seq(20, 70, 4),
  comp_column_name = "length_cm"
)
} # }
```
