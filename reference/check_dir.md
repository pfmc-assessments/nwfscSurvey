# Directory check

Directory check

## Usage

``` r
check_dir(dir, verbose = TRUE)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Details

Check that

1.  The user knows that the data will not be saved if `dir = NULL`.

2.  The directory exists if it can be created.

3.  The function fails if the directory cannot be created.

## Author

Chantel R. Wetzel

## Examples

``` r
check_dir(getwd(), verbose = FALSE)
# See more output
check_dir(getwd())
```
