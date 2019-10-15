#' BATsqlQuery
#'
#' Queries the NWOS DB to select batches of specific records from tables or views
#'
#' @details
#' This function must be run inside an open RODBC channel (32-bit only) connected to the USFS FIA production database through a user with write permissions.
#'
#' @param ids is a character vector containing NWOS CNs to be queried.
#' @param table is a character string containing the name of the table to be queried.
#' @param field is a character string containing the name of the field in the desired table containing the desired CNs.
#' @param idtag is a character string containing the name of the tag in the query template that refers to the ids (for use with custom queries).
#' @param ttag is a character string containing the name of the tag in the query template that refers to the table (for use with custom queries).
#' @param ftag is a character string containing the name of the tag in the query template that refers to the field (for use with custom queries).
#' @param query is a character string containing text of a custom query, containing the values of idtag, ttag, and ftag.
#' @param conn is a character string containing the name of the open RODBC channel.
#'
#' @examples
#' BATsqlQuery(ids=c('SAM0001','SAM0002',table=c('SAMPLE'))
#'
#' @export

BATsqlQuery <- function(ids,
                        table='PLOT_OWNER',
                        field='CN',
						idtag='<I>',
						ttag='<T>',
						ftag='<F>',
                        query="SELECT * FROM FS_NWOS.<T> WHERE <F> IN ('<I>')",
						conn='con'){
  ids <- unique(ids)
  q <- gsub(ttag,table,query)
  q <- gsub(ftag,field,q)
  ch <- split(ids, ceiling(seq_along(ids)/1000))
  for (fi in 1:length(ch)){
    str <- paste(ch[[fi]],collapse="','")
    q2 <- gsub(idtag,str,q)
    ch[[fi]] <- sqlQuery(get(conn),q2)
  }
  ret <- do.call("rbind",ch)
  ret <- unique(ret)
  return(ret)
}