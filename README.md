## nwfscSurvey
[![Build Status](https://travis-ci.org/nwfsc-assess/nwfscSurvey.png?branch=master)](https://travis-ci.org/nwfsc-assess/nwfscSurvey)

Code for analysis of the NWFSC shelf-slope, NWFSC slope, AFSC slope, and Triennial surveys. 
Code within this package allow for pulling of data from the NWFSC data warehouse (https://www.nwfsc.noaa.gov/data) and calculated the design based indices and composition data for use in stock assessment. 

Note: this code was developed for use by scientists at the Northwest Fisheries Science Center and is intended to work on the specific data products that we have access to using methods specific to the needs of this group. 

## Installation

```S
install.packages("devtools")
library(devtools)

devtools::install_github("nwfsc-assess/nwfscSurvey", build_vignettes = TRUE)
```


