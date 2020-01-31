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

  md <- x@fields[x@fields$FIELD_NAME %in% c(names(x@quest),names(x@sample)),]
  #add weights column
  md <- rbind(md,
              data.frame(FIELD_NAME=c('PLOT_COUNT','FINAL_WEIGHT'),
                         DESCRIPTION=c('Number of plots associated with this survey','Sampling weight, for estimates and models'),
                         DATA_TYPE=c('NUMERIC','NUMERIC'),
                         UNITS_FACTORS=c('PLOTS',NA)))
  return(md)
}
