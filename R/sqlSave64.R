#' sqlSave64
#'
#' Wrapper function for RODBC:sqlSave to save data to the NWOS database from a 64-bit R session
#'
#' @details
#' This function must be run on a machine with an ODBC connection (32-bit) to the USFS FIA production database through a user with read permissions.
#'
#' @param data is a dataframe containing records to be loaded to the NWOS DB
#' @param tname is a string containing the table to which the data should be appended
#' @param vtypes is a vector containing the oracle datatype associated with columns in data
#'
#' @return a dataframe containing the return from the SQL query
#'
#' @examples
#' sqlSave64(data=df,tname='FS_NWOS.CODES_UPDATE',vtypes=c(CREATED_DATE="Date"))
#'
#' @export

sqlSave64 <- function(data,tname,vtypes){
  
  #save data in temporary directory, along with strings
  #containing locations of temporary directory, data and script files
  df <- data
  tn <- tname
  vt <- vtypes
  dir <- tempdir()
  data <- paste(dir,'\\querydata.RData',sep='')
  script <- paste(dir,'\\queryscript.R',sep='')
  save(list=c("df","tn","vt","dir","data"),file=data)
  
  #create temporary script to run query in temporary directory
  sink(script)
  cat("options(stringsAsFactors = FALSE) \n")
  cat("library(RODBC) \n")
  cat(paste("load(",gsub("\\","\\\\",data,fixed=T),") \n",sep="'"))
  cat("con <- odbcConnect('FIADB01P') \n")
  cat("sqlSave(con,df,tablename=tn,rownames=F,varTypes=vt,append=T) \n")
  cat("odbcClose(con) \n")
  sink()
  
  #open 32-bit instance and run temporary script
  rl <- paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe") #location of 32-bit R
  rc <- paste(rl,script) #script to be run in 32-bit R
  system(rc) #send full R call to system
}