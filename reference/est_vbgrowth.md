# Estimate length-at-age using the von Bertanlaffy parameterization

Estimate length-at-age using the von Bertanlaffy parameterization

## Usage

``` r
est_vbgrowth(
  data,
  dir = NULL,
  col_length = "length_cm",
  col_age = "age",
  init_params = data.frame(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.1, CV1 = 0.1),
  bins = NULL
)
```

## Arguments

- data:

  Data frame of biological data from [`pull_bio()`](pull_bio.md)

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- col_length:

  A character string specifying the column to use in `data` for length
  information. These lengths are assumed to be in centimeters. The
  default value is `length_cm`.

- col_age:

  A character string specifying the column to use in `data` for age
  information. These ages are assumed to be in years. The default value
  is `age`.

- init_params:

  Data frame of starting parameters for K, Linf, L0, CV0, and CV2 based
  on the Stock Synthesis parameterization of von Bertanlaffy growth.

- bins:

  The bins to put ages into. If NULL then simply uses the ages as
  recorded.

## See also

[`fit_vbgrowth()`](fit_vbgrowth.md)

## Author

Chantel Wetzel
