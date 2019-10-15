#' nwos_wide_metadata
#'
#' Creates metadata table for NWOS data in wide format
#'
#' @param x is a nwos.object
#'
#' @return a data.frame
#'
#' @examples
#' nwos_wide_metadata(foo)
#'
#' @export

nwos_wide_metadata <- function(x){
  
  if(!is(x,'nwos.object')){
    stop("nwos_wide_metadata() requires nwos.object as input")
  }
  
  f <- x@fields[x@fields$FIELD_NAME %in% names(x@sample),] #db fields included
  
  col <- c("QUESTION_NAME","QUESTION_TEXT","DATA_TYPE","UNITS_FACTORS", 
           "ITEM_TYPE","CHECK_GROUP")
  
  md <- x@metadata[1,col] #copy of single metadata record
  md[] <- NA
  ls <- replicate(nrow(f)+2,md,simplify=F)
  md <- do.call("rbind",ls)
  #fill in md from f
  md$QUESTION_NAME <- c(f$FIELD_NAME,'POINT_COUNT','FINAL_WEIGHT')
  md$QUESTION_TEXT <- c(f$DESCRIPTION,'Number of plots associated with this survey','Sampling weight, for estimates and models')
  md$DATA_TYPE <- c(f$DATA_TYPE,'NUMERIC','NUMERIC')
  md$UNITS_FACTORS <- c(f$UNITS_FACTORS,'PLOTS',NA)
  #collapse metadata and md
  md <- rbind(md,x@metadata[col])
  names(md)[1:2] <- c('COLUMN','DESCRIPTION')
  
  #reformat UNITS_FACTORS
  fuf <- function(x){
    ifelse(substr(x,1,2)=='c(',
           paste(paste(matrix(eval(parse(text=x)),ncol=2,byrow=T)[,1],
                       matrix(eval(parse(text=x)),ncol=2,byrow=T)[,2],
                       sep='='),collapse=', '),
           x)
    
  }
  md$UNITS_FACTORS <- sapply(md$UNITS_FACTORS,fuf)
  
  return(md)
}