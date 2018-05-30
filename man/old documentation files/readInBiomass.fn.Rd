\name{readInBiomass.fn}
\alias{readInBiomass.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read in the biomass estimates from csv file provided by survey team
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
readInBiomass.fn(filename, headerRow = 1, sep = ",", colNames = c("Species", "ScientificName", "SpeciesCode", "Year", "Project", "StrataAreaVersion", "AreaSetId", "AreaName", "SouthernLatitude", "NorthernLatitude", "DepthStrataSet", "MinStratumDepth", "MaxStratumDepth", "StratumArea", "Biomass", "Abundance", "CpueWeightVar", "CpueCountVar", "BiomassVar", "CV", "N", "Nbio", "Npos", "NbioPos"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{headerRow}{
%%     ~~Describe \code{headerRow} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
}
  \item{colNames}{
%%     ~~Describe \code{colNames} here~~
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
%function (filename, headerRow = 1, sep = ",", colNames = c("Species", 
%    "ScientificName", "SpeciesCode", "Year", "Project", "StrataAreaVersion", 
%    "AreaSetId", "AreaName", "SouthernLatitude", "NorthernLatitude", 
%    "DepthStrataSet", "MinStratumDepth", "MaxStratumDepth", "StratumArea", 
%    "Biomass", "Abundance", "CpueWeightVar", "CpueCountVar", 
%    "BiomassVar", "CV", "N", "Nbio", "Npos", "NbioPos")) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
