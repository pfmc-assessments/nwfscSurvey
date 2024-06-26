% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetStrata.fn.R
\name{GetStrata.fn}
\alias{GetStrata.fn}
\title{Get Default Strata}
\usage{
GetStrata.fn(area = "coast")
}
\arguments{
\item{area}{A single character value of the area or areas you want to
predict to. Multiple areas must be separated with an underscore, e.g.,
\code{"wa_or"} or \code{"north_south"}.
The default is \code{"coast"}, which provides shallow and deep
strata for each state; thereby, covering the entire US West Coast.}
}
\value{
A data frame with six columns,
\itemize{
\item name,
\item area,
\item Depth_m.1 (m; the shallow depth border),
\item Depth_m.2 (m; the deep depth border),
\item Latitude_dd.1 (decimal degrees; southern border), and
\item Latitude_dd.2 (decimal degrees; northern border).}
}
\description{
Get Default Strata
}
\details{
Get a data frame of strata for design-based estimates and survey
composition data. Several strata are available for different default areas,
as well as combinations using \code{"_"} in the argument \code{area}.
}
\seealso{
This function uses \code{\link{CreateStrataDF.fn}}
to generate the strata returned by this function. Also, see
convert_strata4vast function in the VASTWestCoast package that converts results
from this function to strata that can be used within the VAST package.
}
\author{
Chantel Wetzel and Kelli Faye Johnson
}
