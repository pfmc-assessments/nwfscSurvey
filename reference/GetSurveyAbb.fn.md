# Get Survey Abbreviation

Get Survey Abbreviation

## Usage

``` r
GetSurveyAbb.fn(surveys = "Combo", na.return = "")
```

## Arguments

- surveys:

  A vector of strings specifying the survey names that you want to get
  abbreviations for.

- na.return:

  The desired entry you want to replace all
  [`is.na()`](https://rdrr.io/r/base/NA.html) values with. The default
  is to return values of `""` instead of `NA`, though you can change it
  to actually return NA values by using `na.return = NA` as is, i.e.,
  without quoting NA.

## Value

A vector of strings containing a single abbreviation for each input
value in `surveys`. Only the first match for each survey is returned.
The function returns `""` for surveys not found in the matrix by
default, but this return value for unmatched surveys can be changed by
altering `na.return`.

## Details

Get abbreviations for a vector of survey names. The input vector of
names, which are typically names used to pull the data from the
warehouse, do not always match the name agreed upon within the
Population Ecology Program. So, standard abbreviations are returned for
a given warehouse name. Partial matches are allowed, and are searched
for using the first two columns of [`createMatrix()`](createMatrix.md),
so be weary of using terms used for multiple surveys as only the first
match will be returned for each input value.

## See also

See [`createMatrix()`](createMatrix.md) for a list of available surveys.

## Author

Kelli Faye Johnson

## Examples

``` r
# Return a vector of agreed upon abbreviations for the
# Triennial Survey and the West Coast Groundfish Bottom Trawl Survey.
GetSurveyAbb.fn(c("Triennial", "Combo"))
#> [1] "Triennial Survey" "NWFSC WCGBTS"    
```
