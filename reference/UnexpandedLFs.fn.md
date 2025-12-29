# Creates a matrix of unexpanded length composition data

Creates a matrix of unexpanded length composition data

## Usage

``` r
UnexpandedLFs.fn(
  dir = NULL,
  datL,
  lgthBins = 1,
  sex = 3,
  partition = 0,
  fleet = "Enter Fleet",
  ageErr = "NA",
  agelow = -1,
  agehigh = -1,
  month = "Enter Month",
  printfolder = "forSS3",
  verbose = TRUE
)
```

## Arguments

- dir:

  Directory where output will be saved. The directory where the file
  should be saved. If dir = NULL no output will be saved.

- datL:

  A data frame of length-composition data returned from
  [`pull_bio()`](pull_bio.md).

- lgthBins:

  Vector of length bins to create length compositions across. Values
  above or below the minimum or maximum values, respectively, are
  grouped into the first size or plus group size.

- sex:

  Options of (0, 1, 2, 3). The integer will be used to define the sex
  column of the returned input for Stock Synthesis and specifies how the
  composition are treated with respect to sex. See the Stock Synthesis
  manual for more information. In short, 0 is for unsexed, 1 is females,
  2 is males, and 3 is females and males. The default is `3`.

- partition:

  Partition to assign the composition data based on the expected format
  for Stock Synthesis. Partition of 0 indicates that the composition
  data include all composition data, 1 for discarded composition data,
  and 2 for retained fish only. Default of 0.

- fleet:

  A single integer value. A user input fleet number to assign to the
  fleet column based on the expected format for Stock Synthesis. Default
  "Enter Fleet".

- ageErr:

  Single integer value of ageing error vector to apply to the age data
  based on Stock Synthesis. Default "Enter".

- agelow:

  Lower age bin for all age composition data based on the expected
  format for Stock Synthesis. Default value of -1 which translates to
  the lowest age bin.

- agehigh:

  Upper age bin for all age composition data based on the expected
  format for Stock Synthesis. Default value of -1 which translates to
  the highest

- month:

  A single integer value between 1-12. A user input fleet number to
  assign to the month column based on the expected format for Stock
  Synthesis. See the Stock Synthesis manual for more information.
  Default "Enter Month".

- printfolder:

  A string that will be appended to `dir`, creating a folder where the
  output will be saved. If specified as `""`, the output will just be
  saved directly in `dir`. The default is `"forSS3"`.

- verbose:

  A logical that specifies if you want to print messages and warnings to
  the console. The default is `TRUE`.

## Author

Chantel Wetzel
