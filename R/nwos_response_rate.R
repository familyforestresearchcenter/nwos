#' nwos_response_rate
#'
#' Calculates nwos response rate 
#'
#' @param x is a vector containing values of I ('response'), P ('partial response'), NC ('not contacted'), UN ('unknown'), and R ('refused')
#'
#' @return a numeric value
#'
#' @examples
#' nwos_response_rate(c('I','I','R','R','NC','NC','P'))
#'
#' @export

nwos_response_rate <- function(x){length(x[x=='I'])/length(x)}
