% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Biomass.fn.R
\name{Biomass.fn}
\alias{Biomass.fn}
\title{Calculates design based estimates from survey data for early West Coast surveys (e.g., Triennial, AFSC slope)}
\usage{
Biomass.fn(dir, dat, strat.vars = c("Depth_m", "Latitude_dd"), strat.df,
  printfolder = "forSS", outputMedian = T, verbose = TRUE, convert = 1,
  season = NA, fleet = NA)
}
\arguments{
\item{dir}{directory of the excel file}

\item{dat}{Dataframe of the data. It must have catch rate called catchPerArea and columns corresponding to strata variable names. It must also have a column called year.}

\item{strat.df}{a dataframe with the first column the name of the stratum, the second column the area of the stratum, and the remaining columns are the high and low variables defining the strata.}

\item{printfolder}{the folder where files will be saved}

\item{outputMedian}{T/F output median or the mean biomass estimate}

\item{convert}{convert biomass}

\item{season}{season for SS}

\item{fleet}{fleet number for SS}

\item{stat.vars}{A vector of the strata variable names (i.e., c("BEST_LATITUDE","BEST_DEPTH"))}
}
\description{
The variables defining the strata must begin with the name in strat.vars and end with ".1" or ".2" (i.e., BEST_DEPTH.1)
the strata are assumed to be continuous variables, thus have a lower and upper value defining them. The lower value does not necessarily have to be the same as the previous upper value.
the stat.df dataframe is difficult to build up with more than one variable becuase it turns into a design where you have to define all areas, thus repeat the variables for one (like a design)
}
\details{
An example strat.df
data.frame(name=c("shallowS","shallowN","deepS","deepN"),area=c(1200,1400,2000,2500),BEST_DEPTH.1=c(rep(55,2),rep(100,2)),BEST_DEPTH.2=c(rep(100,2),rep(183,2)),INPFC=c(32,42,32,42),BEST_LATITUDE.2=c(42,49,42,49))
I should think about adding in a routine that automatically puts in the latitude if stratifying by INPFC (or simply finds the strata by INPFC)
The code below splits out the data by stratum and calculates the average density in each stratum. It then expands up by area to give an estimated weight in each stratum.
The stratum weights are added together to get a total index for that year
I calculate the variance given stratified sampling theory
I work in normal space, then calculate the statistics if B is lognormal
This is the Mean Ratio Estimate
}
\author{
Allan Hicks and Chantel Wetzel
}