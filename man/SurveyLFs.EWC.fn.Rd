\name{SurveyLFs.EWC.fn}
\alias{SurveyLFs.EWC.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate length frequency compositions
}
\description{
Calculate length frequency compositions from Early West Coast surveys for input into SS3
}
\usage{
SurveyLFs.EWC.fn(datL, datTows, strat.vars = NULL, strat.df = NULL, femaleMale = c(2, 1), lgthBins = 1, SS3out = F, meanRatioMethod = T, gender = 3, NAs2zero = T, sexRatioUnsexed = NA, maxSizeUnsexed = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datL}{
    Dataframe of the length observations. It should have columns called 'year', 'SEX', 'LENGTH', 'HAULJOIN', 'WEIGHT', 'NUMBER_FISH', 'DISTANCE_FISHED', 'NET_WIDTH', and \code{strat.vars}
}
  \item{datTows}{
    Dataframe of tow data.  It shoud have columns named 'year', and \code{strat.vars}
}
  \item{strat.vars}{
    Character vector of the stratum variables. Typically 'BOTTOM_DEPTH' and 'START_LATITUDE'. See Details.
}
  \item{strat.df}{
    A dataframe describing the strata name, area, and boundaries.  Boundaries are determined by latitude and bottom depth. See Details.
}
  \item{femaleMale}{
    A vector of codes for female then male (in that order). Default c(2,1)
}
  \item{lgthBins}{
    The increment of each length bin or a vector of the actual bins
}
  \item{SS3out}{
    Logical indicating whether or not to put output in SS3 format. Default FALSE.
}
  \item{meanRatioMethod}{
    Use the meanRatioMethod or totalRatioMethod. Default TRUE for meanRatioMethod.
}
  \item{gender}{
    Gender type for SS3. Gender=0 is unsexed, gender=3 is female and males separate.
}
  \item{NAs2zero}{
    Set any NAs in length composition (lack of data) to zero. Default TRUE.
}
  \item{sexRatioUnsexed}{
    The assumed sex ratio for fish up to maxSizeUnsexed to apply to unsexed fish when gender=3. This is used for fish up to length \code{maxSizeUnsexed}.  Default NA does not reapportion unsexed fish into the length composition.
}
  \item{maxSizeUnsexed}{
    The maximum size to apply \code{sexRatioUnsexed}. Sizes larger than this use the data to determine sex ratio.
}
}
\details{
   The dataframes of survey catch data and length data are usually downloaded from the AFSC servers.
   
   strat.df is a dataframe with a column called ``name'' and two columns for each stratum variable (START_LATITUDE and BOTTOM_DEPTH) indicating the low and high bounds. 
   The columns for each stratum variable should be named with the stratum variable name followed by .1 for the low bound and .2 for the high bound. For example,
   this is what a strat.df would look like (as called in the example below).
   \begin{verbatim}   
   name  area    START_LATITUDE.2 START_LATITUDE.1 BOTTOM_DEPTH.1 BOTTOM_DEPTH.2
   A     5829    49.0             45.0             183            549
   B     4024    49.0             45.0             549            900
   C     9259    49.0             40.5             900            1280
   D     6211    45.0             40.5             183            549
   E     5264    45.0             40.5             549            900
   F     6952    40.5             34.5             183            549
   G     7801    40.5             34.5             549            900
   H     8059    40.5             34.5             900            1280
   \end{verbatim}
   You can use \code{\link{StratAreas.fn}} to calculate the area of the strata using the SA3 data. You will need to supply boundaries available in the SA3 file.

   The length bin called F.999 or M.999 is retained to show proportion of lengths smaller than smallest bin. 
   You will want to likely add this to your first length bin and delete this before putting in SS3, or
   start the lgthBins argument at the 2nd length bin and F.999 will be all fish smaller (hence the first length bin)


}
\value{
   if \code{SS3out} is FALSE, then it returns a list with
  \item{L.year}{Yearly length compositions}
  \item{L.year.str}{Yearly length compositions by stratum}

   if \code{SS3out} is TRUE, then it returns a dataframe with yearly length compositions in the format ready to paste into SS3. Remove F.999 and M.999 if necessary.

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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
   \dontrun{
     xB <- datB[datB$SURVEY=="AFSC.Slope" & datB$YEAR%in%1997:2001,]
     xB$year <- xB$YEAR
     xL <- datL[datL$SURVEY=="AFSC.Slope" & datL$YEAR%in%1997:2001,]
     xL <- xL[!is.na(xL$LENGTH),]
     xL$WEIGHT <- xL$SP_TOW_WGHT_KG
     xL$NUMBER_FISH <- xL$SP_TOW_NUM
     xL$year <- xL$YEAR

     LFs <- SurveyLFs.EWC.fn(xL,xB,strat.vars=c("BOTTOM_DEPTH","START_LATITUDE"),strat.df=strata.AKslope,femaleMale=c(2,1),lgthBins=seq(80,600,20),SS3out=T,gender=3,sexRatioUnsexed=0.5,maxSizeUnsexed=200)
     #the length comps ready for SS3, by sex (use gender=0 for unsexed)
     SurveyLFs.EWC.fn(xL,xB,strat.vars=c("BOTTOM_DEPTH","START_LATITUDE"),strat.df=strata.AKslope,femaleMale=c(2,1),lgthBins=c(80,600),SS3out=T,gender=3,sexRatioUnsexed=0.5,maxSizeUnsexed=200)
     SurveyLFs.EWC.fn(xL,xB,strat.vars=c("BOTTOM_DEPTH","START_LATITUDE"),strat.df=strata.AKslope,femaleMale=c(2,1),lgthBins=seq(10,80,10),SS3out=T,gender=3,sexRatioUnsexed=0.5,maxSizeUnsexed=200)
   }

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
