#' maxcn
#'
#' Identifies NWOS CN with the highest numerical component
#'
#' @param x is a character vector of NWOS CNs
#'
#' @return a numeric vector
#'
#' @examples
#' maxcn(c('SAM00001','SAM04567'))
#'
#' @export

maxcn <- function(x){paste(substr(x[1],1,3),max(ncn(x),na.rm=T),sep="")}
