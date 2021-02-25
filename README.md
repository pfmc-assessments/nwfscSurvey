<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/nwfsc-assess/nwfscSurvey/workflows/R-CMD-check/badge.svg)](https://github.com/nwfsc-assess/nwfscSurvey/actions)
[![DOI](https://zenodo.org/badge/26344817.svg)](https://zenodo.org/badge/latestdoi/26344817)
<!-- badges: end -->

## Installation

`nwfscSurvey` provides code for analysis of the NWFSC shelf-slope, NWFSC
slope, AFSC slope, and Triennial surveys. Code within this package allow
for pulling of data from the NWFSC data warehouse
(<https://www.nwfsc.noaa.gov/data>) and calculated the design based
indices and composition data for use in stock assessment.

Note: this code was developed for use by scientists at the Northwest
Fisheries Science Center and is intended to work on the specific data
products that we have access to using methods specific to the needs of
this group.

``` r
install.packages("remotes")
remotes::install_github("nwfsc-assess/nwfscSurvey")
```

A short vignette for the package is:

``` r
vignette("nwfscSurvey", package = "nwfscSurvey")
```
