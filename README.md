## nwfscSurvey
Code for analysis of the NWFSC shelf-slope, NWFSC slope, AFSC slope, and Triennial surveys.

Note: this code was developed for use by scientists at the Northwest Fisheries Science Center and is intended to work on the specific data products that we have access to using methods specific to the needs of this group. Anyone interested in obtaining survey data should contact the NWFSC to make a formal data request.

## Installation

```S
install.packages("devtools")
library(devtools)

devtools::install_github("nwfsc-assess/nwfscSurvey", ref = "development", build_vignettes = TRUE)
```

A vignette with example commands can be viewed using the command
```S
vignette("nwfscSurvey")
```


## Overview
The following functions can be used to generate length comps for the NWFSC shelf-slope bottom trawl survey

```S    
# Read length comps
Data = readInLengthComps.fn(...)
    
# Plot sex ratio
plotSexRatio.fn( len=Data )
    
# Process data
SS3 = SS3LF.fn(len=Data, lgthBins=seq(14,66,by=2))
```
