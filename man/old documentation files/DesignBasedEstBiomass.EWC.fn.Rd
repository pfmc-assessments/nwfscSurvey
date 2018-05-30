\name{DesignBasedEstBiomass.EWC.fn}
\alias{DesignBasedEstBiomass.EWC.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Calculate yearly biomass estimates
}
\description{
 Calculates yearly estimates of biomass using design-based expansion. 
}
\usage{
 DesignBasedEstBiomass.EWC.fn(dat, strat.vars, strat.df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
     A dataframe of survey catch data with a column called catchPerArea
  }
  \item{strat.vars}{
    A character vector of the names of the strata variables.  They must be associated with column names in \code{strat.df}. See details for more information.
}
  \item{strat.df}{
    A dataframe describing the strata name, area, and boundaries.  Boundaries are determined by latitude and bottom depth. See Details.
}
}
\details{
   The dataframe of survey catch data is usually downloaded from the AFSC servers.  See example below how to calculate catchPerArea.
   
   strat.df is a dataframe with a column called ``name'' and two columns for each stratum variable (START_LATITUDE and BOTTOM_DEPTH) indicating the low and high bounds. 
   The columns for each stratum variable should be named with the stratum variable name followed by .1 for the low bound and .2 for the high bound. For example,
   this is what a strat.df would look like (as called in the example below).

   name  area    START_LATITUDE.2 START_LATITUDE.1 BOTTOM_DEPTH.1 BOTTOM_DEPTH.2
   A     5829    49.0             45.0             183            549
   B     4024    49.0             45.0             549            900
   C     9259    49.0             40.5             900            1280
   D     6211    45.0             40.5             183            549
   E     5264    45.0             40.5             549            900
   F     6952    40.5             34.5             183            549
   G     7801    40.5             34.5             549            900
   H     8059    40.5             34.5             900            1280

   You can use \code{\link{StratAreas.fn}} to calculate the area of the strata using the SA3 data. You will need to supply boundaries available in the SA3 file.
}
\value{
Strata=yearlyStrataEsts,Total=ests,LNtons=ln
    \item{Strata }{Strata specific estimates}
    \item{Total }{Estimates summed over all strata}
    \item{LNtons }{Natural log of the total for input into SS3}


}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Allan Hicks
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{StratAreas.fn}}, \code{\link{ReadInBiomass.EWC.fn}}, \code{\link{SA3}},  
}
\examples{

\dontrun{
 ### AFSC slope
 #datB is read in using ReadInBiomass.EWC.fn
 xB <- datB[datB$SURVEY=="AFSC.Slope" & datB$YEAR%in%1997:2001,]
 xB$year <- xB$YEAR
 #calculate the density (kg/km^2) using net width and distance fished
 xB$areaFished <- xB$DISTANCE_FISHED*(xB$NET_WIDTH/1000)
 tmp <- sum(is.na(xB$areaFished))
 cat("There are",tmp,"instances where area swept could not be calculated due to missing values.\n")
 xB$catchPerArea <- xB$WEIGHT/xB$areaFished
 xB$catchPerArea[is.na(xB$catchPerArea)&!is.na(xB$areaFished)] <- 0                                  #the tows with no observation of the species
 xB <- xB[!is.na(xB$catchPerArea),]
 #call the function for the Design based estimates
 #use biomass$LNtons for SS3 (with medianBhat)
 biomass <- DesignBasedEstBiomass.EWC.fn(xB,strat.vars=c("BOTTOM_DEPTH","START_LATITUDE"),strat.df=strata.AKslope)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
