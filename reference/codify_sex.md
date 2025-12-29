# Code sexes into standardized strings

Information for sexes can be stored in several forms. `codify_sex()`
works to standardize all possible input values for sex into one of three
character values, i.e., `F`, `M`, or `U`. And, `codify_sex_SS3()` runs
`codify_sex()` on the input and then further standardizes `F` and `M` to
3 and all other values to 0 to match grouping of sexed and unsexed fish
for Stock Synthesis.

## Usage

``` r
codify_sex(x)

codify_sex_SS3(x)
```

## Arguments

- x:

  A vector of values used to store sex information. Can be any
  combination of integers, single characters, or `NA` values.

## Value

A vector of `F`, `M`, or `U` values the same length as `x` or a vector
of `0L` or `3L` for `codify_sex()` and `codify_sex_SS3()`, respectively.

## Codifying

Pattern matching is used via
[`grepl()`](https://rdrr.io/r/base/grep.html) to facilitate matching
both standard and potentially erroneous matches. For example, the code
can accommodate any number of white space characters before the text
string via `"^\\s*`. Additionally, only the first letter of the
character string is of importance because the code is strictly looking
for versions of `female`, `male`, and `unsexed`, ignoring case. All `NA`
values are coded as `U` for unsexed. A manual produced by Washington
Department of Fish and Wildlife,
<https://wdfw.wa.gov/sites/default/files/publications/01754/wdfw01754.pdf>,
provides some of the following information regarding how various
sampling programs codify sex.

- The Alaska Fisheries Science Center Slope Survey stored sexes uses
  integers, i.e., male (1), female (2), and unsexed (3).

- Washington followed the same format as the Alaska Fisheries Science
  Center Slope Survey.

- Oregon and California also use 1 for males and 2 for females; but 9 is
  used for unknown sex.

## Message

A message is printed to the screen if any values do not have a map to
`F`, `M`, or `U`. For example, if `happy` was an entry in `x`, a summary
of the number of times happy was seen in x would be printed to the
screen via `message` and `happy` would be coded to `U` such that the
returned value is still viable but the user is made aware that something
is potentially wrong in `x`. See for yourself in the examples.

## Author

Kelli F. Johnson

## Examples

``` r
# All values are successfully coded
codify_sex(c("U", "F", "M", 1, 2))
#> [1] "U" "F" "M" "M" "F"
# Some values are not initially successfully coded and
# warning messages are printed to the screen prior to
# changing the uncoded values to "U"
codify_sex(c("U", "F", "M", "both", 1, 2, NA))
#> The following unmatched values were found n times in `codify_sex()`:
#> 'both' (n = 1)
#> 'NA' (n = 1)
#> [1] "U" "F" "M" "U" "M" "F" "U"
codify_sex(c("both", rep(5, 10), "both", 1, 2, NA))
#> The following unmatched values were found n times in `codify_sex()`:
#> '5' (n = 10)
#> 'both' (n = 2)
#> 'NA' (n = 1)
#>  [1] "U" "U" "U" "U" "U" "U" "U" "U" "U" "U" "U" "U" "M" "F" "U"

# Change codified sex from letters to integers for Stock Synthesis
codify_sex_SS3(c("M", "U", "K"))
#> The following unmatched values were found n times in `codify_sex()`:
#> 'K' (n = 1)
#> [1] 3 0 0
```
