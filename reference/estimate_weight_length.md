# Calculate weight-length relationship parameters

Calculate weight-length relationship parameters

## Usage

``` r
estimate_weight_length(
  data,
  col_length = "length_cm",
  col_weight = "weight_kg",
  verbose = FALSE
)
```

## Arguments

- data:

  A data frame containing empirical weights and lengths from sampled
  fish. Sexes should be available in the column `sex` or `SEX`.

- col_length:

  A numeric or character value specifying the column to use in `data`
  for length information. These lengths are assumed to be in
  centimeters. The default value is `length_cm`.

- col_weight:

  A numeric or character value specifying the column to use in `data`
  for weight information. These weights are assumed to be in kilograms
  The default value is `weight_kg`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

A data frame of weight-length parameters by sex. Parameters A and B are
in the appropriate units to input into Stock Synthesis Wtlen_1_Fem and
Wtlen_2_Fem, or Wtlen_1_Mal and Wtlen_1_Mal, parameters in the control
file. Values of `NA` are returned for models that did not have enough
data to properly estimate the parameters. This will happen when there
are no females in your data set, for example.

## Details

Estimate parameters of the weight-length relationship for each sex and
all sexes combined, where the latter includes unsexed fish.

## Author

Kelli F. Johnson and Chantel Wetzel
