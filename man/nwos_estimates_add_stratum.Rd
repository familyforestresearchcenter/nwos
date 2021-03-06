% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nwos_estimates_add_stratum.R
\name{nwos_estimates_add_stratum}
\alias{nwos_estimates_add_stratum}
\title{Add Stratum Variables to an NWOS Data Set}
\usage{
nwos_estimates_add_stratum(x = NA, data = QUEST)
}
\arguments{
\item{x}{list number. Only applicable if is data is a list of data frames, instead of a single data frame. This used mainly for apply functions.}

\item{data}{data frame or list of data frames}
}
\description{
Add variables to an NWOS data frame
}
\details{
The default values create the variables used in the NWOS tables.
}
\examples{
nwos_estimates_add_stratum()
}
\keyword{nwos}
