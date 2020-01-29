#' nwos_full
#'
#' Combines nwos plots and response data to recreate the full sample
#'
#' @param quest is a nwos.object
#' @param plots is a nwos.plots.object
#' @param imputations is string containing which imputation set is to be used in place of non-response, if any. Valid responses are "none","random","1","2","3","4","5"
#'
#' @return a data.frame
#'
#' @examples
#' nwos_full(quest,plots,imputations="1")
#'
#' @export

nwos_full <- function(quest, plots, imputations="none"){

  if(!is(quest,'nwos.object')){
    stop("'quest' requires nwos.object as input")
  }

  if(!is(plots,'nwos.plots.object')){
    stop("'plots' requires nwos.plots.object as input")
  }

  if(!imputations %in% c('none','random',1:5)){
    stop("'imputations' only excepts values of 'none','random', or integer")
  }

  q <- nwos_wide(quest,imputations=imputations) #individual wide datasets
  p <- nwos_plots_complete(plots)

  if (any(!p$CN %in% q$RESPONSE_CN)){
    stop("'nwos.plots.object' and 'nwos.object' do not appear to be from the same sample")
  }

  pc <- c(1:3,5:9,11:12) #column indices to merge
  qc <- c(1,3:4,7:259)

  pq <- merge(p[,pc],q[,qc],by.x="RESPONSE_CN",by.y="CN",all.x=T)
  pq <- pq[,c(names(p[,pc]),names(q[,qc[2:256]]))] #rename

  return(pq)
}
