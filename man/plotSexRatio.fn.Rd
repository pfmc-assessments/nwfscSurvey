\name{plotSexRatio.fn}
\alias{plotSexRatio.fn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plots the sex ratio by length bins
}
\description{
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotSexRatio.fn(len, fn = median, circleSize = 0.1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{len}{
    Data frame with length data (must have columns 'Length','NumF', and
    'NumM')
  }
  \item{fn}{
    Function used in 'lapply'. Default=median.
  }
  \item{circleSize}{
    Scalar for size of circles in figure. Default = 0.1.
  }
  \item{...}{
    Further arguments to be passed to 'plot'.
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
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.
 
%## The function is currently defined as
%function (len, fn = median, circleSize = 0.1) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
