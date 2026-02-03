<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/pfmc-assessments/nwfscSurvey/workflows/R-CMD-check/badge.svg)](https://github.com/pfmc-assessments/nwfscSurvey/actions)
[![DOI](https://zenodo.org/badge/26344817.svg)](https://zenodo.org/badge/latestdoi/26344817)
<!-- badges: end -->

## Installation

`nwfscSurvey` provides code for analysis of the Northwest Fisheries Science Center (NWFSC) West Coast Groundfish Bottom Trawl, NWFSC
slope, Alaska Fisheries Science Center (AFSC) slope, and AFSC Triennial surveys. Code within this package allows
for pulling of data from the NWFSC data warehouse (currently, unavailable)), calculating the design-based
indices of abundance, visualization of data, and processing length- and age-composition data for use in West Coast groundfish stock assessment.

This code was developed for use by scientists at the NWFSC and is intended to work on the specific data products that we have access to using methods specific to the needs of this group.

``` r
install.packages("remotes")
remotes::install_github("pfmc-assessments/nwfscSurvey")
```

## Package information

A [website](http://pfmc-assessments.github.io/nwfscSurvey/index.html) is now available for the package. The ["Get started"](http://pfmc-assessments.github.io/nwfscSurvey/articles/nwfscSurvey.html) tab provide general information on how to pull, visualize, and process survey data. Additionally, the ["Reference"](http://pfmc-assessments.github.io/nwfscSurvey/reference/index.html) tab provides a detailed list of all available functions. 
