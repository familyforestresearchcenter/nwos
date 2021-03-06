% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nwos_export.R
\name{nwos_export}
\alias{nwos_export}
\title{nwos_export}
\usage{
nwos_export(x, dir = "~", format = "wide", imputations = "none")
}
\arguments{
\item{x}{is a nwos.object}

\item{dir}{is the directory to export to}

\item{format}{is a character string containing the values 'wide','long', or 'full'}

\item{imputations}{is string containing which imputation set is to be used in place of non-response, if any. Valid responses are "none","random","1","2","3","4","5"}
}
\value{
a data.frame
}
\description{
Exports NWOS data and metadata as CSVs, in multiple formats.
}
\examples{
nwos_export(foo,dir='~',format='long',imputations="1")

}
