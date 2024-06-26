% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_url.R
\name{get_url}
\alias{get_url}
\title{Function to create url to pull data from the data warehouse}
\usage{
get_url(data_table, project_long, add_species, years, vars_long)
}
\arguments{
\item{data_table}{table to pull from the data warehouse, options =
trawl.catch_fact, trawl.operation_haul_fact}

\item{project_long}{survey project name}

\item{add_species}{string of species name created by the pull_catch or pull_bio
functions.}

\item{years}{An integer vector of length two with the
range of years to pull data for (e.g., c(2003, 2024)).
Vector can not contain -Inf or Inf.}

\item{vars_long}{string of fields to pull from the data warehouse}
}
\description{
Function to create url to pull data from the data warehouse
}
\author{
Chantel Wetzel
}
