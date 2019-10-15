#' nxtcn
#'
#' Identifies the numerically subsequent NWOS CN number, for appending new records to a table
#'
#' @param x is a character vector of NWOS CNs
#'
#' @return a numeric vector
#'
#' @examples
#' maxcn(c('SAM00001','SAM04567'))
#'
#' @export

nxtcn <- function(x) {paste(substr(x[1],1,3),(ncn(maxcn(x))+1),sep="")}
