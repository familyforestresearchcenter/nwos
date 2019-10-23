#' nwos_cooperation_rate
#'
#' Calculates nwos cooperation rate (~AAPOR's COOP1)
#'
#' @param x is a vector containing values of I ('response'), P ('partial response'), NC ('not contacted'), UN ('unknown'), and R ('refused')
#'
#' @return a numeric value
#'
#' @examples
#' nwos_cooperation_rate(c('I','I','R','R','NC','NC','P'))
#'
#' @export

nwos_cooperation_rate <- function(x){length(x[x=='I'])/length(x[x %in% c('I','P','R')])}
