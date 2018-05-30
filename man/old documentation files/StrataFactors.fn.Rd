\name{StrataFactors.fn}
\alias{StrataFactors.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Strata factors by name
}
\description{
Creates a vector of strata factors by name
}
\usage{
StrataFactors.fn(dat, strat.vars, strat.df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
    A dataframe with column names corresponding to \code{strat.vars}
}
  \item{strat.vars}{
    Character vector of the stratum variables. Typically 'BOTTOM_DEPTH' and 'START_LATITUDE'. See Details.
}
  \item{strat.df}{
    A dataframe describing the strata name, area, and boundaries.  Boundaries are determined by latitude and bottom depth. See Details.
}
}
\details{
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
    A vector of stratum names as a factor
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
