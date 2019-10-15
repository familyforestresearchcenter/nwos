#' nwos_long_metadata
#'
#' Creates metadata table for NWOS data in long format
#'
#' @param x is a nwos.object
#'
#' @return a data.frame
#'
#' @examples
#' nwos_long_metadata(foo)
#'
#' @export

nwos_long_metadata <- function(x){
  
  if(!is(x,'nwos.object')){
    stop("nwos_long_metadata() requires nwos.object as input")
  }
  
  md <- x@fields[x@fields$FIELD_NAME %in% names(x@quest),]
  return(md)
}