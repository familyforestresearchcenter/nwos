#' tableformat
#'
#' Formats NWOS DB tables, both full tables and update tables
#'
#' @details
#' This function must be run on a machine with an ODBC connection (32-bit) to the USFS FIA production database through a user with read permissions.
#'
#' @param type a character vector equal to 'full' or 'update'.
#'
#' @examples
#' tableformat(type='full')
#'
#' @export

tableformat <- function(type='update'){
  
  #changing global settings
  options(stringsAsFactors = FALSE)
  options(scipen=999)
  
  #download fields metadata from DB
  fields <- sqlQuery64("SELECT * FROM FS_NWOS.FIELDS")
  
  if (type=='update'){ #creates vector of table names, either standard or _UPDATE tables
    tn <- paste(unique(fields$TABLE_NAME),"_UPDATE",sep="")
  } else if (type=='full') {
    tn <- unique(fields$TABLE_NAME)
  } else {
    stop("type must be equal to 'full' or 'update'")
  }
  
  for (i in 1:length(tn)){ #scroll through all tables
    if(tn[i] %in% ls(envir=.GlobalEnv)){ #determine if table i is in memory
      fsub <- subset(fields,TABLE_NAME==gsub('_UPDATE','',tn[i])) #subset fields
	  fsub <- fsub[order(fsub$COLUMN),] #order fsub
      df <- get(tn[i]) #make copy of table i
	  if (nrow(df)!=length(unique(df$CN))){
		warning(paste("Duplicates in ",tn[i],".",sep=""))
	  }
	  df <- df[,fsub$FIELD_NAME] #constrain and order fields to fsub
      for (j in 1:nrow(fsub)){ #scroll through all columns and coerce to correct data type
        if (fsub$DATA_TYPE[j] %in% c('CHAR255','CHAR2000')){
          df[,fsub$FIELD_NAME[j]] <- as.character(df[,fsub$FIELD_NAME[j]]) #to character
        } else if (fsub$DATA_TYPE[j] == 'DATE'){
          df[,fsub$FIELD_NAME[j]] <- as.Date(df[,fsub$FIELD_NAME[j]]) #to date
        } else if (fsub$DATA_TYPE[j] == 'INT'){
          df[,fsub$FIELD_NAME[j]] <- as.integer(df[,fsub$FIELD_NAME[j]]) #to integer
        } else if (fsub$DATA_TYPE[j] == 'NUM'){
          df[,fsub$FIELD_NAME[j]] <- as.numeric(df[,fsub$FIELD_NAME[j]]) #to number
        }
      }
      assign(tn[i],df,envir=.GlobalEnv) #assign df to table i in global
    }
    
  }

}