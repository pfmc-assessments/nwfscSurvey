\name{SurveyAgeAtLen.EWC.fn}
\alias{SurveyAgeAtLen.EWC.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate and format age-at-length compositions
}
\description{
Calculates age-at-length compositions from Early West Coast survey data and formats it for SS3.
}
\usage{
SurveyAgeAtLen.EWC.fn(datAL, datTows, strat.vars = NULL, strat.df = NULL, femaleMale = c(2, 1), lgthBins = 1, ageBins = 1, SS3out = F, meanRatioMethod = T, raw = T, NAs2zero = T, season = "ENTER", fleet = "ENTER", partition = "ETNER", ageerr = "ENTER", returnSamps = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datAL}{
    Dataframe of the length and age observations. It should have columns called 'year', 'SEX', 'LENGTH', 'AGE', 'HAULJOIN', 'WEIGHT', 'NUMBER_FISH', 'DISTANCE_FISHED', 'NET_WIDTH', and \code{strat.vars}
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
  \item{ageBins}{
    The increment of each age bin or a vector of the actual bins
}
  \item{SS3out}{
    Logical indicating whether or not to put output in SS3 format. Default FALSE.
}
  \item{meanRatioMethod}{
    Use the meanRatioMethod or totalRatioMethod. Default TRUE for meanRatioMethod.
}
  \item{raw}{
    Logical to use raw age at length. If FALSE it will use expanded ages. Default TRUE
}
  \item{NAs2zero}{
    Set any NAs in length composition (lack of data) to zero. Default TRUE.
}
  \item{season}{
    A single number to insert in the SS3 output for season
}
  \item{fleet}{
    A single number to insert in the SS3 output for fleet
}
  \item{partition}{
    A single number to insert in the SS3 output for partition
}
  \item{ageerr}{
    A single number to insert in the SS3 output for ageing error
}
  \item{returnSamps}{
    Logical if TRUE returns a list of sample sizes for females and males instead of age-at-length output. Default FALSE.
}
}
\details{
   The dataframes of survey catch data, length data, and age data are usually downloaded from the AFSC servers.
   
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

}
\value{
   if \code{SS3out} is FALSE, then it returns a list with
  \item{AL.year}{Yearly age-at-length compositions}
  \item{A.year.L.str}{Yearly age-at-length compositions by stratum}

   if \code{SS3out} is TRUE, then it returns a list with female and male dataframes with yearly age-at-length compositions in the format ready to paste into SS3. Remove F.999 and M.999 if necessary.
   
   if \code{returnSamps} is TRUE then a list with the number of observations for females and males.

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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
