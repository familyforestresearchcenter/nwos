#' nwos_cooperation_rate
#'
#' Calculates nwos cooperation rate 
#'
#' @param x is a vector containing values of R ('response'), NR ('non-response'), and ENR ('excused non-response' or undeliverable)
#'
#' @return a numeric value
#'
#' @examples
#' nwos_cooperation_rate(c('R','R','NR','NR','ENR','ENR','ENR'))
#'
#' @export

nwos_cooperation_rate <- function(x){length(x[x=='R'])/length(x[x %in% c('R','NR')])}