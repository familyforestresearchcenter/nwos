#' nwos_export
#'
#' Exports NWOS data and metadata as CSVs, in either long or wide format.
#'
#' @param x is a nwos.object
#' @param dir is the directory to export to
#' @param format is a character string containing the values 'wide' or 'long'
#'
#' @return a data.frame
#'
#' @examples
#' nwos_export(foo,dir='~',format='long')
#'
#' @export

nwos_export <- function(x,dir='~',format='wide'){
  
  if(!is(x,'nwos.object')){
    stop("nwos_export() requires nwos.object as input")
  }
  
  if(! format %in% c('wide','long')){
    stop("nwos_export() only accepts 'wide' or 'long' as 'format'")
  }
  
  n <- paste(dir,'/QUEST_',x@sample$NWOS_CYCLE[1],'.csv',sep='')
  n2 <- paste(dir,'/QUEST_',x@sample$NWOS_CYCLE[1],'_METADATA.csv',sep='')
  
  if (format=='wide'){
    t <- nwos_wide(x)
    m <- nwos_wide_metadata(x)
  } else if (format=='long'){
    t <- nwos_long(x)
    m <- nwos_long_metadata(x)
  }
  
  write.csv(t,n,row.names=F,na='')
  write.csv(m,n2,row.names=F,na='')
  
}