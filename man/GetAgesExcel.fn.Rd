\name{GetAgesExcel.fn}
\alias{GetAgesExcel.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Reads in age data from Excel file and returns age frequencies in SS3 format
}
\description{
calls readInExcelAgeComps.fn, then calls SS3AgeAtLen.fn
}
\usage{
GetAgesExcel.fn(file, sheet = "AgeComps", headerRow, lgthBins = 1, ageBins = 1, nSamps = "EnterNsamps", fleet = "EnterFleet", season = 1, partition = 0, ageerr = "EnterAgeErr", raw = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{sheet}{
%%     ~~Describe \code{sheet} here~~
}
  \item{headerRow}{
%%     ~~Describe \code{headerRow} here~~
}
  \item{lgthBins}{
%%     ~~Describe \code{lgthBins} here~~
}
  \item{ageBins}{
%%     ~~Describe \code{ageBins} here~~
}
  \item{nSamps}{
%%     ~~Describe \code{nSamps} here~~
}
  \item{fleet}{
%%     ~~Describe \code{fleet} here~~
}
  \item{season}{
%%     ~~Describe \code{season} here~~
}
  \item{partition}{
%%     ~~Describe \code{partition} here~~
}
  \item{ageerr}{
%%     ~~Describe \code{ageerr} here~~
}
  \item{raw}{
%%     ~~Describe \code{raw} here~~
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
%function (file, sheet = "AgeComps", headerRow, lgthBins = 1, 
%    ageBins = 1, nSamps = "EnterNsamps", fleet = "EnterFleet", 
%    season = 1, partition = 0, ageerr = "EnterAgeErr", raw = T) 
%{
%    age <- readInExcelAgeComps.fn(file, sheet = sheet, headerRow = headerRow)
%    SS3AgeAtLen.fn(age, lgthBins = lgthBins, ageBins = ageBins, 
%        fleet = fleet, season = season, partition = partition, 
%        ageerr = ageerr, raw = raw)
%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
