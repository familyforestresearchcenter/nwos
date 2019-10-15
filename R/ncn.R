#' ncn
#'
#' Extracts numeric component of NWOS CN
#'
#' @param x is a character vector of NWOS CNs
#'
#' @return a numeric vector
#'
#' @examples
#' ncn(c('SAM00001','OWN04567'))
#'
#' @export

ncn <- function(x){as.numeric(substr(x,4,nchar(x)))}