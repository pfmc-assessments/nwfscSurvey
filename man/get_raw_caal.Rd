% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_raw_caal.R
\name{get_raw_caal}
\alias{get_raw_caal}
\title{Calculate unexpanded/raw length or marginal age compositions}
\usage{
get_raw_caal(
  data,
  len_bins,
  age_bins,
  length_column_name = "length_cm",
  age_column_name = "age",
  dir = NULL,
  month = "Enter Month",
  fleet = "Enter Fleet",
  partition = 0,
  ageerr = "Enter Numeric",
  printfolder = "forSS3",
  verbose = TRUE
)
}
\arguments{
\item{data}{A data frame that includes columns of year, sex, and length/ages. The data
frame can be survey data pulled using pull_bio from the data warehouse or any data frame
that includes column names of sex, year, and the comp_column_name.  The sex column is
expected to have sexes denoted by F, M, and U.}

\item{length_column_name}{The length column name to create conditional age-at-length
data for. The default is \code{length_cm}.}

\item{age_column_name}{The age column name to create conditional age-at-length
data for. The default is \code{age}.}

\item{dir}{Directory where output will be saved. The directory where the file should be saved.
If dir = NULL no output will be saved.}

\item{month}{Month the samples were collected based on the expected format for
Stock Synthesis to determine the length/age estimate to compare to. Default
"Enter Month".}

\item{fleet}{A fleet number to assign the composition data to based on the expected
format for Stock Synthesis. Default "Enter Fleet".}

\item{partition}{Partition to assign the composition data based on the expected
format for Stock Synthesis. Partition of 0 indicates that the composition data
include all composition data, 1 for discarded composition data, and 2 for retained
fish only. Default of 0.}

\item{ageerr}{Number of ageing error vector to apply to the age data based on
Stock Synthesis. Default "Enter Numeric".}

\item{printfolder}{A string that will be appended to \code{dir}, creating a folder
where the output will be saved. If specified as \code{""},
the output will just be saved directly in \code{dir}. The default is \code{"forSS3"}.}

\item{verbose}{A logical that specifies if you want to print messages and
warnings to the console. The default is \code{TRUE}.}
}
\value{
A data frame of conditiona age-at-length compositions for sexed and
unsexed fish formatted for Stock Synthesis.
}
\description{
Calculate unexpanded/raw length or marginal age compositions
}
\details{
Creates a matrix of unexpanded (or raw) marginal length or age composition
data formatted for Stock Synthesis. The code will return composition data
for all sexes present in the data frame and no sex assignment is done for
unsexed fish. The function will create composition data for either
lengths or ages based on the comp_column_name. The function will return a
list of composition data based upon the sexes present in the data for a
two-sex model or all length/ages for single-sex model.
}
\examples{
\dontrun{
bio <- pull_bio(
  common_name = "lingcod",
  survey = "NWFSC.Combo"
)

caal_data <- get_raw_caal(
  data = bio,
  len_bins = seq(20, 70, 4),
  age_bins = 1:30,
)
}

}
\author{
Chantel Wetzel
}
