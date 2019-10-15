#' nwos_response_rate
#'
#' Calculates nwos response rate 
#'
#' @param x is a vector containing values of R ('response'), NR ('non-response'), and ENR ('excused non-response' or undeliverable)
#'
#' @return a numeric value
#'
#' @examples
#' nwos_response_rate(c('R','R','NR','NR','ENR','ENR','ENR'))
#'
#' @export

nwos_response_rate <- function(x){length(x[x=='R'])/length(x)}