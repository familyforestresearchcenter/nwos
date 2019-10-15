#' nwos_long
#'
#' Formats NWOS data into a long format for analysis
#'
#' @param x is a nwos.object
#'
#' @return a data.frame
#'
#' @examples
#' nwos_long(foo)
#'
#' @export

nwos_long <- function(x){
  
  if(!is(x,'nwos.object')){
    stop("nwos_long() requires nwos.object as input")
  }
  
  l <- merge(x@sample,x@quest,by="CN")
  #reformat l
  for (i in 1:length(l)){ #cast columns as correct datatype
    t <- x@fields$DATA_TYPE[match(names(l)[i],x@fields$FIELD_NAME)]
    if (t=='CHAR255'){
      l[,i] <- as.character(l[,i])
    } else if (t=='INT'){
      l[,i] <- as.integer(l[,i])
    }
  }
  return(l)
}