# Save an object in an `.rdata` file

Save an object in an `.rdata` file

## Usage

``` r
save_rdata(x, dir = NULL, name_base, verbose = TRUE)
```

## Arguments

- x:

  An object.

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- name_base:

  A string that will be appended to with the system time and the file
  extension (i.e., `".rdata"`).

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Value

Nothing is returned, instead a file is saved in `dir`.
