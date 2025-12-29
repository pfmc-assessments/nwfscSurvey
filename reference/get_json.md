# Get json content from a URL

Get json content from a URL

## Usage

``` r
get_json(url)
```

## Arguments

- url:

  A string containing a valid URL to pull the data from the data
  warehouse.

## Value

A data frame.

## Details

Get information stored on the web in .json format using a URL. The
content is first pulled from the web as text with UTF-8 encoding. Then
the text is passed to
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).
This workflow ensures that the URL is not mistaken for a file name
rather than web content.

## See also

See all the `pull_*` functions for examples where this function is used,
e.g., [`pull_catch()`](pull_catch.md).

## Author

Kelli F. Johnson
