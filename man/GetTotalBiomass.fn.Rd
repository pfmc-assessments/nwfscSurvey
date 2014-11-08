\name{GetTotalBiomass.fn}
\alias{GetTotalBiomass.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Output the biomass in SS3 format, reading in from a csv file
}
\description{
A wrapper for to output the biomass in SS3 format, reading in from a csv file

}
\usage{
GetTotalBiomass.fn(file, headerRow, fleet = "EnterFleet", season = 1, outputMedian = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{headerRow}{
%%     ~~Describe \code{headerRow} here~~
}
  \item{fleet}{
%%     ~~Describe \code{fleet} here~~
}
  \item{season}{
%%     ~~Describe \code{season} here~~
}
  \item{outputMedian}{
%%     ~~Describe \code{outputMedian} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
 
%## The function is currently defined as
%function (file, headerRow, fleet = "EnterFleet", season = 1, 
%    outputMedian = T) 
%{
%    bio <- readInBiomass.fn(file, headerRow)
%    SS3Biomass.fn(bio, fleet, season)
%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
