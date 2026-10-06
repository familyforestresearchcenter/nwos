#' get_nwos_ancillary
#'
#' Returns ancillary data, in raw or aggregated form, for use with raw NWOS data.
#'
#' @details
#' This function must be run on a machine with an ODBC connection to the USFS FIA production database through a user with read permissions. If the function is run with no variable listed in the ‘var’ parameter, it will return the metadata for all ancillary datasets in the database. This table is useful as a lookup table. Otherwise, the ‘by’ parameter determines how the returned data are aggregated. If we select ‘by=all’ (the default), the raw values are returned for all sample points. For numeric variables, however, we will usually want to aggregate the values by survey (for surveys with more than one sample point) in order to attach to a wide NWOS table.
#'
#' @param var is a string containing the name of the ancillary variable to be returned.
#' @param by is a function for aggregating point-level values, e.g. 'mean','min','max', etc.
#'
#' @return a data.frame
#'
#' @examples
#' get_nwos_ancillary(var="CENSUS",by="all")
#' get_nwos_ancillary(var="ACREAGE",by="mean")
#'
#' @export

get_nwos_ancillary <- function(var=NA,by="all"){
  
  #changing global settings
  options(stringsAsFactors = FALSE)
  options(scipen=999)
  
  if (!by %in% c('mean','max','min','all')){
    stop("'by' parameter must include one of 'mean','max','min', or 'all'")
  }
  
  md <- read.csv("T:/FS/RD/FIA/NWOS/DB/OFFLINE_TABLES/_REF_ANCILLARY_METADATA.csv")
  
  if (!var %in% md$VARNAME & !is.na(var)){
    em <- "'var' is not a valid ancillary dataset in NWOS-DB"
    em <- gsub('var',var,em)
    stop(em)
  }
  
  if (is.na(var)){ #if no variable is listed, return metadata table (i.e. lookup table)
    df <- md[2:4]
  } else if (by=="all"){ #if by=all, return all sample points
    ad <- read.csv("T:/FS/RD/FIA/NWOS/DB/OFFLINE_TABLES/_REF_ANCILLARY_DATA.csv")
    ad$VARNAME <- md$VARNAME[match(ad$AMD_CN,md$CN)]
    ad <- ad[ad$VARNAME==var,c(2:4,6)]
    names(ad)[4] <- var
    df <- ad
  } else { #otherwise, aggregate by RESPONSE_CN
    ad <- read.csv("T:/FS/RD/FIA/NWOS/DB/OFFLINE_TABLES/_REF_ANCILLARY_DATA.csv")
    ad <- ad[!is.na(ad$RESPONSE_CN),]
    ad$VARNAME <- md$VARNAME[match(ad$AMD_CN,md$CN)]
    ad <- ad[ad$VARNAME==var,c(3,6)]
    ad$VALUE <- as.numeric(ad$VALUE)
    ad2 <- aggregate(VALUE~RESPONSE_CN,ad,by)
    names(ad2)[2] <- var
    df <- ad2
  } 
 
  df[df==""] <- NA #empty string to null
  return(df)   
}