% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeSeltxt.R
\name{makeSeltxt}
\alias{makeSeltxt}
\title{makeSeltxt}
\usage{
makeSeltxt(
  size_csv,
  size_col = "Measured.Length..cm.",
  fishery_col = "Fishery",
  time_col = "Year",
  sex = F,
  sex_col = "Sex",
  sex_vals = c("F", "M"),
  lBins = seq(50, 250, 5)
)
}
\arguments{
\item{size_csv}{csv with capture information; each row should be a fish, sex optional}

\item{size_col}{character name of column with measurement information; will drop NA rows}

\item{fishery_col}{character name of column with fishery names; used to form cuts}

\item{time_col}{character name of column by which time should be grouped; currently only supports one value (e.g. 'year')}

\item{sex}{logical. should we aggregate by sex? if so, the resultant text file will print females and males in that order}

\item{sex_col}{character name of column with sexes}

\item{sex_vals}{vector of sex designations, female then male}

\item{lBins}{vector of bin breaks}
}
\description{
\code{makeSeltxt} reshapes a dataframe with size-at-capture data into a selectivity text file suitable for SS3
}
