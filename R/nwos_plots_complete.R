#' nwos_plots_complete
#'
#' Creates a 'completed' form of the NWOS plots table 
#'
#' @param x is a nwos.plots object
#'
#' @return a data.frame
#'
#' @examples
#' nwos_plots_complete(foo)
#'
#' @export

nwos_plots_complete <- function(x){
  
  if(!is(x,'nwos.plots.object')){
    stop("nwos_wide_raw() requires nwos.object as input")
  }
  
  merge(x@plots,
    x@response_codes[,c('SKEY','SAMPLE_CN','RESPONSE_CN','RESPONSE_CAT')],
    by='SKEY',all.x=T)[2:13]
}
