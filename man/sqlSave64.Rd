% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sqlSave64.R
\name{sqlSave64}
\alias{sqlSave64}
\title{sqlSave64}
\usage{
sqlSave64(data, tname, vtypes)
}
\arguments{
\item{data}{is a dataframe containing records to be loaded to the NWOS DB}

\item{tname}{is a string containing the table to which the data should be appended}

\item{vtypes}{is a vector containing the oracle datatype associated with columns in data}
}
\value{
a dataframe containing the return from the SQL query
}
\description{
Wrapper function for RODBC:sqlSave to save data to the NWOS database from a 64-bit R session
}
\details{
This function must be run on a machine with an ODBC connection (32-bit) to the USFS FIA production database through a user with read permissions.
}
\examples{
sqlSave64(data=df,tname='FS_NWOS.CODES_UPDATE',vtypes=c(CREATED_DATE="Date"))

}
