#' sqlQuery64
#'
#' Wrapper function for RODBC:sqlQuery to send queries to the NWOS database from a 64-bit R session
#'
#' @details
#' This function must be run on a machine with an ODBC connection (32-bit) to the USFS FIA production database through a user with read permissions.
#'
#' @param query is a string containing the text of the SQL query to be sent to the NWOS DB
#'
#' @return a dataframe containing the return from the SQL query
#'
#' @examples
#' sqlQuery64(query='SELECT * FROM FS_NWOS.FIELDS')
#'
#' @export

sqlQuery64 <- function(query){
  
  #save query in temporary directory, along with strings
  #containing locations of temporary directory, data and script files
  q <- query
  dir <- tempdir()
  data <- paste(dir,'\\querydata.RData',sep='')
  script <- paste(dir,'\\queryscript.R',sep='')
  save(list=c("q","dir","data"),file=data)
  
  #create temporary script to run query in temporary directory
  sink(script)
  cat("options(stringsAsFactors = FALSE) \n")
  cat("library(RODBC) \n")
  #cat("print(Sys.getenv('R_ARCH')) \n")
  cat(paste("load(",gsub("\\","\\\\",data,fixed=T),") \n",sep="'"))
  cat("con <- odbcConnect('FIADB01P') \n")
  cat("r <- sqlQuery(con,q) \n")
  cat("odbcClose(con) \n")
  cat("save(list=c('r'),file=data) \n")
  sink()
  
  #open 32-bit instance and run temporary script
  rl <- paste0(Sys.getenv("R_HOME"), "/bin/x64/Rscript.exe") #location of 32-bit R
  rc <- paste(rl,script) #script to be run in 32-bit R
  system(rc) #send full R call to system
  
  load(data) #load data returned from 32-bit instance
  return(r) #return query returns to environment
}