#' allunique
#'
#' Determines whether a vector contains only unique, non-null values
#'
#' @param x is a vector
#'
#' @return a logical value
#'
#' @examples
#' allunique(c("A","A","B"))
#'
#' @export

allunique <- function(x) {length(x)==lunique(na.omit(x))}