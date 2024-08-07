% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetN.fn.R
\name{GetN.fn}
\alias{GetN.fn}
\title{Calculate input sample sizes}
\usage{
GetN.fn(
  dir = NULL,
  dat,
  type = c("length", "age"),
  species = c("all", "flatfish", "shelfrock", "sloperock", "thorny", "others"),
  printfolder = "forSS3",
  output = NULL,
  verbose = TRUE
)
}
\arguments{
\item{dir}{Directory where output will be saved. The directory where the file should be saved.
If dir = NULL no output will be saved.}

\item{dat}{A data frame of composition data created using \code{\link[=pull_bio]{pull_bio()}}.}

\item{type}{A string specifying whether doing "length" or "age" that is
used to ensure the sample size is of the correct column and create
the file name of the saved sheet.}

\item{species}{A string specifying the species group of interest, which
will lead to the use of the correct species-specific value for
the number of unique samples per tow. See the function call for
allowed values, where the default is \code{"all"}.}

\item{printfolder}{A string that will be appended to \code{dir}, creating a folder
where the output will be saved. If specified as \code{""},
the output will just be saved directly in \code{dir}. The default is \code{"forSS3"}.}

\item{output}{A string, where the default is \code{NULL}, which returns
only a vector of samples sizes.
\code{"summary"}, or any other character string, will return
a table of observations by year and sex.}

\item{verbose}{A logical that specifies if you want to print messages and
warnings to the console. The default is \code{TRUE}.}
}
\description{
Calculate input sample sizes
}
\examples{
\dontrun{
bio <- pull_bio(
  common_name = "petrale sole",
  survey = "NWFSC.Combo"
)

n <- GetN.fn(
  dat = bio,
  type = "length",
  species = "flatfish"
)
}

}
\references{
Stewart, I.J. and O.S. Hamel. 2014.
Bootstrapping of sample size for length- or age-composition data used in
stock assessment.
Canadian Journal of Fishery and Aquatic Science, 71(4): 581--588.
\href{https://doi.org/10.1139/cjfas-2013-0289}{10.1139/cjfas-2013-0289}.
}
\author{
Chantel R. Wetzel
}
