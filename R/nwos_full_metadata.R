#' nwos_full_metadata
#'
#' Creates metadata table for full (i.e. responses and plots) NWOS dataset
#'
#' @param quest is a nwos.object
#' @param plots is a nwos.plots.object
#' 
#' @return a data.frame
#'
#' @examples
#' nwos_full_metadata(foo)
#'
#' @export

nwos_full_metadata <- function(quest, plots){
  
  if(!is(quest,'nwos.object')){
    stop("'quest' requires nwos.object as input")
  }
  
  if(!is(plots,'nwos.plots.object')){
    stop("'plots' requires nwos.plots.object as input")
  }
  
  pc <- c(1:3,5:9,11:12) #column indices to include
  qc <- c(3:4,7:259)
  
  p <- nwos_plots_metadata(plots)[pc,]
  q <- nwos_wide_metadata(quest)[qc,]
  
  pq <- rbind(p,q)
  
  return(pq)
}
