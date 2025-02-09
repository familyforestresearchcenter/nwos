#' export_update_table
#'
#' Exports an update table dataframe as a CSV along with a SQL file to the T drive, to manually update NWOS-DB
#'
#' @details
#' This function must be run on a machine with a connection to USFS T drive
#'
#' @param df is a string containing the name of a NWOS-DB update table
#'
#' @examples
#' export_update_table(df="SAMPLE_UPDATE")
#'
#' @export

export_update_table <- function(df){
  file <- paste("T:/FS/RD/FIA/NWOS/DB/UPDATE_FILES/",df,".csv",sep="")
  write.csv(get(df),file,row.names=F,na="")
  stub <- gsub("_UPDATE","",df)
  query <- "SELECT * FROM FS_NWOS.<STUB>_UPDATE;\nDELETE FROM FS_NWOS.<STUB> WHERE CN IN (SELECT CN FROM FS_NWOS.<STUB>_UPDATE);\nINSERT INTO FS_NWOS.<STUB> (SELECT DISTINCT * FROM FS_NWOS.<STUB>_UPDATE);\nDELETE FROM FS_NWOS.<STUB>_UPDATE"
  query <- gsub("<STUB>",stub,query)
  file <- paste("T:/FS/RD/FIA/NWOS/DB/UPDATE_FILES/",df,".SQL",sep="")
  sink(file)
  cat(query)
  sink()
  message(paste("Files in T:/FS/RD/FIA/NWOS/DB/UPDATE_FILES/. Load ",df,".csv directly to ",df," table in PLSQL and then run ",df,".SQL to load.",sep=""))
}