#' NWOSdelete
#'
#' Deletes records from the NWOS database
#'
#' @details
#' This function must be run inside an open RODBC channel (32-bit only) connected to the USFS FIA production database through a user with write permissions.
#'
#' @param tables is a character vector containing names of vectors currently in the working environment; each vector contains CNs to be deleted from the corresponding table.
#'
#' @examples
#' NWOSdelete(tables=c('RESPONSE_DELETE'))
#'
#' @export

NWOSdelete <- function(tables){
  q <- "DELETE FROM FS_NWOS.<TAB> WHERE CN IN ('<CNS>')"
  for (i in 1:length(tables)){  
    q2 <- gsub("<TAB>",gsub("_DELETE","",tables[i]),q)
    t <- get(tables[i])
    ls <- split(t,ceiling(seq_along(t)/300))
    for (j in 1:length(ls)){
      q3 <- gsub('<CNS>',paste(ls[[j]],collapse="','"),q2)
      sqlQuery(con,q3)
    }
	message(paste("'",tables[i],"' REMOVED FROM DATABASE.",sep=""))
  }
}