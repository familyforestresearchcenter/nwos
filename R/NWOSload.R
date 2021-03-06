#' NWOSload
#'
#' Loads update tables to the NWOS database
#'
#' @details
#' This function must be run inside an open RODBC channel (32-bit only) connected to the USFS FIA production database through a user with write permissions.
#'
#' @param tables is a character vector containing names of update tables currently in the working environment.
#'
#' @examples
#' NWOSload(tables=c('SAMPLE_UPDATE','OWNER_UPDATE'))
#'
#' @export

NWOSload <- function(tables) {

  #identify special variable types
  #download metadata
  FIELDS <- sqlQuery(con,"SELECT * FROM FS_NWOS.FIELDS WHERE DATA_TYPE IN ('CHAR2000','DATE')")
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
      sqlSave(con, df, tablename=tnu,
              rownames=F, varTypes=vb[[i]][[2]], append=T)
      #delete old records
      q <- "DELETE FROM <TN> WHERE CN IN (SELECT CN FROM <TNU>)"
      q <- gsub("<TN>",tn,q)
      q <- gsub("<TNU>",tnu,q)
      sqlQuery(con,q)
      #insert new records
      q <- "INSERT INTO <TN> (SELECT DISTINCT * FROM <TNU>)"
      q <- gsub("<TN>",tn,q)
      q <- gsub("<TNU>",tnu,q)
      sqlQuery(con,q)
      #clear _UPDATE table
	  q <- "DELETE FROM <TNU>"
      q <- gsub("<TNU>",tnu,q)
      sqlQuery(con,q)
	  message(paste("'",vb[[i]][1],"' LOADED.",sep=""))
    }
  }

}
