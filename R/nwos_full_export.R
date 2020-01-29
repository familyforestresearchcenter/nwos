#' nwos_full_export
#'
#' Exports full NWOS dataset and metadata as CSVs, in a wide format.
#'
#' @param quest is a nwos.object
#' @param plots is a nwos.plots.object
#' @param dir is the directory to export to
#' @param imputations is string containing which imputation set is to be used in place of non-response, if any. Valid responses are "none","random","1","2","3","4","5"
#'
#' @return a data.frame
#'
#' @examples
#' nwos_full_export(foo,foo2,dir='~',format='long',imputations="1")
#'
#' @export

nwos_full_export <- function(quest,plots,dir='~',imputations="none"){

  if(!is(quest,'nwos.object')){
    stop("nwos_export() requires nwos.object as input")
  }

  if(!is(plots,'nwos.plots.object')){
    stop("'plots' requires nwos.plots.object as input")
  }

  if(!imputations %in% c('none','random',1:5)){
    stop("'imputations' only excepts values of 'none','random', or integer")
  }

  n <- paste(dir,'/QUEST_',quest@sample$NWOS_CYCLE[1],'.csv',sep='')
  n2 <- paste(dir,'/QUEST_',quest@sample$NWOS_CYCLE[1],'_METADATA.csv',sep='')

  t <- nwos_full(quest=quest,plots=plots,imputations=imputations)
  m <- nwos_full_metadata(quest=quest,plots=plots)

  write.csv(t,n,row.names=F,na='')
  write.csv(m,n2,row.names=F,na='')

}
