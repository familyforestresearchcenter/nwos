#' nwos_export
#'
#' Exports NWOS data and metadata as CSVs, in multiple formats.
#'
#' @param x is a nwos.object
#' @param dir is the directory to export to
#' @param format is a character string containing the values 'wide','long', or 'full'
#' @param imputations is string containing which imputation set is to be used in place of non-response, if any. Valid responses are "none","random","1","2","3","4","5"
#'
#' @return a data.frame
#'
#' @examples
#' nwos_export(foo,dir='~',format='long',imputations="1")
#'
#' @export

nwos_export <- function(x,dir='~',format='wide',imputations="none"){

  if(!is(x,'nwos.object')){
    stop("nwos_export() requires nwos.object as input")
  }

  if(! format %in% c('wide','long','full')){
    stop("nwos_export() only accepts 'wide','long', or 'full' as 'format'")
  }

  if(!imputations %in% c('none','random',1:5)){
    stop("'imputations' only excepts values of 'none','random', or integer")
  }

  n <- paste(dir,'/QUEST_',x@sample$NWOS_CYCLE[1],'.csv',sep='')
  n2 <- paste(dir,'/QUEST_',x@sample$NWOS_CYCLE[1],'_METADATA.csv',sep='')

  if (format=='wide'){
    t <- nwos_wide(x,imputations=imputations)
    m <- nwos_wide_metadata(x)
  } else if (format=='long'){
    t <- nwos_long(x,imputations=imputations)
    m <- nwos_long_metadata(x)
  } else if (format=='full'){
    t <- nwos_full(x,imputations=imputations)
    m <- nwos_full_metadata(x)
  }

  write.csv(t,n,row.names=F,na='')
  write.csv(m,n2,row.names=F,na='')

}
