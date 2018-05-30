\name{ReadInLengths.EWC.fn}
\alias{ReadInLengths.EWC.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Read in length data for the Early West Coast surveys.
}
\description{
  A function to read in length data for the Early West Coast surveys from a csv file.
}
\usage{
ReadInLengths.EWC.fn(dataFile, directory, species = c(NA), removeCAN = T, verbose = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{dataFile}{
     The name of the csv data file.
}
  \item{directory}{
     Directory where the data file is.
}
  \item{species}{
     A vector of species codes.  Default NA.
}
  \item{removeCAN}{
    Logical indicating whether or not to remove tows that occurred in Canadian waters. Default TRUE.
}
  \item{verbose}{
    Logical indicating whether to provide additional messages to the screen. Default FALSE.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a dataframe with only non-NA lengths.
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
