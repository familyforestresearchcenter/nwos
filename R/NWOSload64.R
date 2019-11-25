#' NWOSload64
#'
#' Loads update tables to the NWOS database from a 64-bit install of R
#'
#' @details
#' This function must be run on a machine with an ODBC connection (32-bit) to the USFS FIA production database through a user with read permissions.
#'
#' @param tables is a character vector containing names of update tables currently in the working environment.
#'
#' @examples
#' NWOSload64(tables=c('SAMPLE_UPDATE','OWNER_UPDATE'))
#'
#' @export

NWOSload64 <- function(tables) {

  #identify special variable types
  #download metadata
  FIELDS <- sqlQuery64("SELECT * FROM FS_NWOS.FIELDS WHERE DATA_TYPE IN ('CHAR2000','DATE')")
  FIELDS <- subset(FIELDS,DATA_TYPE %in% c('VARCHAR2(2000)','DATE'))
  tn <- unique(FIELDS$TABLE_NAME)
  vb <- vector("list",length(tn)) #empty list of names and special variable types
  for (i in 1:length(vb)){
    fsub <- subset(FIELDS,TABLE_NAME==tn[i])
    vt <- ifelse(fsub$DATA_TYPE=='DATE','Date','Varchar(2000)')
    names(vt) <- fsub$FIELD_NAME
    vb[[i]] <- list(paste(tn[i],"_UPDATE",sep=""),vt) #add name of update table / variable list to vb
  }

  #scroll through vb and load those in tables
  for (i in 1:length(vb)){
    if (vb[[i]][1] %in% tables){
      tnu <- paste("FS_NWOS.",vb[[i]][[1]],sep="") #update table name
      tn <- gsub("_UPDATE","",tnu) #full table name
      #load _UPDATE table
	    df <- get(vb[[i]][[1]])
  	  if (nrow(df)!=length(unique(df$CN))){
  		  stop(paste(vb[[i]][1],"has missing or duplicate CNs. Loading has been discontinued."))
  	  }
      sqlSave64(data=df, tname=tnu, vtypes=vb[[i]][[2]])
      #delete old records
      q <- "DELETE FROM <TN> WHERE CN IN (SELECT CN FROM <TNU>)"
      q <- gsub("<TN>",tn,q)
      q <- gsub("<TNU>",tnu,q)
      sqlQuery64(q)
      #insert new records
      q <- "INSERT INTO <TN> (SELECT DISTINCT * FROM <TNU>)"
      q <- gsub("<TN>",tn,q)
      q <- gsub("<TNU>",tnu,q)
      sqlQuery64(q)
      #clear _UPDATE table
      q <- "DELETE FROM <TNU>"
      q <- gsub("<TNU>",tnu,q)
      sqlQuery64(q)
      message(paste("'",vb[[i]][1],"' LOADED.",sep=""))
    }
  }

}
