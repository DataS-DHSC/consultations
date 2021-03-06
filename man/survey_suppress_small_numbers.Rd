% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey_suppress_small_numbers.R
\name{survey_suppress_small_numbers}
\alias{survey_suppress_small_numbers}
\title{Suppress small groups in data}
\usage{
survey_suppress_small_numbers(column, min_n = 10)
}
\arguments{
\item{column}{vector of values to modify}

\item{min_n}{minimum group size to keep}
}
\value{
vector of values with rare values replaced
}
\description{
Replaces values that appear fewer than min_n times with "Other (Aggregated)".
This ensures that in later summaries, these answers are grouped together.
This serves to help prevent statistical disclosure.
However, continue to check your outputs for small cells and aggregate up where necessary.
}
\examples{
survey_suppress_small_numbers(c(rep("test", 15), rep("test2", 5), NA), min_n = 10)
}
