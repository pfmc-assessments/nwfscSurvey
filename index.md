## Installation

`nwfscSurvey` provides code for analysis of the Northwest Fisheries
Science Center (NWFSC) West Coast Groundfish Bottom Trawl, NWFSC slope,
Alaska Fisheries Science Center (AFSC) slope, and AFSC Triennial
surveys. Code within this package allows for pulling of data from the
[NWFSC data warehouse](https://www.nwfsc.noaa.gov/data), calculating the
design-based indices of abundance, visualization of data, and processing
length- and age-composition data for use in West Coast groundfish stock
assessment.

This code was developed for use by scientists at the NWFSC and is
intended to work on the specific data products that we have access to
using methods specific to the needs of this group.

``` r
install.packages("remotes")
remotes::install_github("pfmc-assessments/nwfscSurvey")
```

## Package information

A [website](http://pfmc-assessments.github.io/nwfscSurvey/index.md) is
now available for the package. The [“Get
started”](http://pfmc-assessments.github.io/nwfscSurvey/articles/nwfscSurvey.md)
tab provide general information on how to pull, visualize, and process
survey data. Additionally, the
[“Reference”](http://pfmc-assessments.github.io/nwfscSurvey/reference/index.md)
tab provides a detailed list of all available functions.
