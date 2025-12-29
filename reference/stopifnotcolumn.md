# Stop if column name is not present in the data

Similar in function to
[stopifnot](https://rdrr.io/r/base/stopifnot.html) but specific to a
column name that you wish to double check if it is present in your data
or not.

## Usage

``` r
stopifnotcolumn(data, string)
```

## Arguments

- data:

  A data frame with column names.

- string:

  A character string that you want to know if it exists in the column
  names of `data`.

## Value

`TRUE` is invisibly returned if the column name is found and an
informative [stop](https://rdrr.io/r/base/stop.html) call is initiated
if the column is not present.
