#' BATsqlQuery64
#'
#' Wrapper function for RODBC:sqlQuery to select batches of specific records from tables or views
#'
#' @details
#' This function must be run on a machine with an ODBC connection (32-bit) to the USFS FIA production database through a user with read permissions.
#'
#' @param ids is a character vector containing NWOS CNs to be queried.
#' @param table is a character string containing the name of the table to be queried.
#' @param field is a character string containing the name of the field in the desired table containing the desired CNs.
#' @param idtag is a character string containing the name of the tag in the query template that refers to the ids (for use with custom queries).
#' @param ttag is a character string containing the name of the tag in the query template that refers to the table (for use with custom queries).
#' @param ftag is a character string containing the name of the tag in the query template that refers to the field (for use with custom queries).
#' @param query is a character string containing text of a custom SQL query, containing the values of idtag, ttag, and ftag.
#'
#' @return a dataframe containing the DB return
#'
#' @examples
#' BATsqlQuery64(ids=c('SAM0001','SAM0002',table=c('SAMPLE'))
#'
#' @export


#queries specific records from tables in batches
#when more than 1000 records are identified (1000
#is the max in a single query)
BATsqlQuery64 <- function(ids,
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
    ch[[fi]] <- sqlQuery64(q2)
  }
  ret <- do.call("rbind",ch)
  ret <- unique(ret)
  return(ret)
}