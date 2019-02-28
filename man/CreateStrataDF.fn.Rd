% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createStrataDF.fn.R
\name{CreateStrataDF.fn}
\alias{CreateStrataDF.fn}
\title{Function that is able to create strata data frame and will calculate strata areas using the StrataAreas.fn}
\usage{
CreateStrataDF.fn(names = NA, depths.shallow, depths.deep, lats.south,
  lats.north)
}
\arguments{
\item{names}{vector of names for strata, if not specified the default with be A, B, ...}

\item{lats.south}{vector of the southern latitude splits for each strata}

\item{lats.north}{vector of the northern latitude splits for each strata}

\item{depth.shallow}{vector of the shallow depths splits for each strata}

\item{depth.deep}{vector of the deep depths splits for each strata}
}
\value{
Returns the dataframe input with a column called area.
}
\description{
Example function call
strata = CreateStrataDF.fn(names          = c("deep_s", "shallow_s", "deep_mn", "shallow_m", "shallow_n"), 
                           depths.shallow = c(183,  55, 183, 55, 55),
                           depths.deep    = c(549, 183, 549, 183, 183),
                           lats.south     = c( 32,  32,  40,  40, 44),
                           lats.north     = c( 40,  40,  49,  44, 49))
}
\seealso{
\code{\link{StrataAreas.fn}}
}
\author{
Allan Hicks and Chantel Wetzel
}