% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nwos_table_make_forestarea.R
\name{nwos_table_make_forestarea}
\alias{nwos_table_make_forestarea}
\title{nwos_table_make_forestarea}
\usage{
nwos_table_make_forestarea(AREA)
}
\description{
nwos_table_make_forestarea
}
\examples{
AREA_AL <- readRDS("INPUTS/ESTIMATES/NWOS_FOREST_AREA_2018_20190909.RDS") \%>\% filter(STATECD == 1)
nwos_table_make_forestarea(readRDS("INPUTS/ESTIMATES/NWOS_FOREST_AREA_2018_20190909.RDS"))
}
