% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_stats.R
\name{extract_stats}
\alias{extract_stats}
\title{Extract statistics from a simulation}
\usage{
extract_stats(p_t_vec, threshold = 1e-04, burn_in = 500)
}
\arguments{
\item{p_t_vec}{A numeric vector}

\item{threshold}{A numeric value greater than 0}

\item{burn_in}{A numeric value greater than 0}
}
\value{
A tibble
}
\description{
Extracts statistics from a vector of p_t values
}
\examples{
extract_stats(runif(1000))
}
