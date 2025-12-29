# Calculate input sample sizes

This function calculates the number of unique tows, samples, and input
sample size based on alternative approaches. The input sample size then
is used within the [`get_expanded_comps()`](get_expanded_comps.md) and
[`get_raw_comps()`](get_raw_comps.md) when creating formatted
composition data files for Stock Synthesis.

## Usage

``` r
get_input_n(
  dir = NULL,
  data,
  comp_column_name = "length_cm",
  input_n_method = c("stewart_hamel", "tows", "total_samples"),
  species_group = c("all", "flatfish", "shelfrock", "sloperock", "thorny", "other"),
  printfolder = "forSS3",
  verbose = TRUE
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- data:

  A data frame of composition data created using
  [`pull_bio()`](pull_bio.md).

- comp_column_name:

  The column name to create composition data for. This column can be is
  used to determine whether to format the composition data for length or
  age compositions by looking for either age (e.g., `age_years`, `Age`,
  `best_age`) or length (e.g., `Length`, `length`, `Length_cm`,
  `length_cm`) in the comp_column_name. The comp_column_name is not case
  sensitive. The default is `length_cm`.

- input_n_method:

  Determines the default input sample size to add to the composition
  data for SS3. There are three options: c("stewart_hamel", "tows",
  "total_samples") where the default is "stewart_hamel".

- species_group:

  A string specifying the species group of interest, which will lead to
  the use of the correct species-specific value for the number of unique
  samples per tow. See the function call for allowed values, where the
  default is `"all"`.

- printfolder:

  A string that will be appended to `dir`, creating a folder where the
  output will be saved. If specified as `""`, the output will just be
  saved directly in `dir`. The default is `"forSS3"`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## References

Stewart, I.J. and O.S. Hamel. 2014. Bootstrapping of sample size for
length- or age-composition data used in stock assessment. Canadian
Journal of Fishery and Aquatic Science, 71(4): 581–588.
[10.1139/cjfas-2013-0289](https://doi.org/10.1139/cjfas-2013-0289).

## Author

Chantel R. Wetzel

## Examples

``` r
if (FALSE) { # \dontrun{
bio <- pull_bio(
  common_name = "petrale sole",
  survey = "NWFSC.Combo"
)

n <- get_input_n(
  data = bio,
  comp_column_name = "length_cm",
  input_sample_size_method = "stewart_hamel",
  species_group = "flatfish"
)
} # }
```
