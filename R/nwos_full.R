#' nwos_full
#'
#' Formats full NWOS dataset (i.e. all plots and responses) into a wide format, for use as input in the 'survey' package
#'
#' @param x is a nwos.object
#' @param imputations is string containing which imputation set is to be used in place of non-response, if any. Valid responses are "none","random","1","2","3","4","5"
#'
#' @return a data.frame
#'
#' @examples
#' nwos_full(foo,imputations="1")
#'
#' @export

nwos_full <- function(x,imputations='none'){

  if(!is(x,'nwos.object')){
    stop("nwos_full() requires nwos.object as input")
  }

  p <- expand_table(x@plots,col="NUM_PLOTS")[1:3] #create plots table
  p$STATECD_NWOS <- as.character(p$STATECD_NWOS)
  p$STSTRATA <- paste(p$STATECD_NWOS,p$COND_STATUS_CD,p$OWNCD_NWOS,sep='_')
  
  q <- nwos_wide(x,imputations=imputations) #create wide quest table
  q <- q[!is.na(q$PLOT_COUNT),]
  q <- expand_table(q,col="PLOT_COUNT")
  q$STSTRATA <- paste(q$STATECD_NWOS,'1',q$OWNCD_NWOS,sep='_')
  q <- q[,!names(q) %in% c('STATECD_NWOS','OWNCD_NWOS')]
  
  pq <- merge_many_to_many(x=p,y=q,by.x='STSTRATA',by.y='STSTRATA')
  pq <- pq[,!grepl('STSTRATA',names(pq))]
  pq$NWOS_CYCLE <- na.omit(pq$NWOS_CYCLE)[1] #add cycle to all plots
  
  return(pq)
}