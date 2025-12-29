# Cleans triennial survey data by year and area

Cleans triennial survey data by year and area

## Usage

``` r
ReadInAges.fn(dat, subset_years = NULL, verbose = TRUE)
```

## Arguments

- dat:

  data file name

- subset_years:

  specify the years to retain, default is NULL which will provide 1977,
  alternative input would be 1980:2002 to remove only 1977.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Details

Reads in the West Coast Triennial survey data and filters the data into
what is necessary. It reads in data and makes sure only the species
necessary are kept may want to keep NA (blank in Excel) to select the
zero tows removeCAN is a flag if you want tows in Canadian waters
removed.

Necessary column names SPECIES_CODE AGE

## Author

Allan Hicks and Chantel Wetzel
