#' lunique
#'
#' Calculates number of unique values in a vector
#'
#' @param x is a vector
#'
#' @return a numeric value
#'
#' @examples
#' lunique(c("A","A","B"))
#'
#' @export

lunique <- function(x) {length(unique(x))}
