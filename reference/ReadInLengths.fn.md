# Reads in the compositional survey data and filters the data into what is necessary It reads in catch and station data and makes sure only the species necessary are kept

### may want to keep NA (blank in Excel) to select the zero tows

removeCAN is a flag if you want tows in Canadian waters removed need the
file called foreign_hauls.csv

Necessary column names SPECIES_CODE LENGTH

## Usage

``` r
ReadInLengths.fn(dat, verbose = TRUE)
```

## Arguments

- dat:

  data file name

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Author

Allan Hicks and Chantel Wetzel
