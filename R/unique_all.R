#' unique.all
#'
#' Determines whether a vector contains only unique, non-null values
#'
#' @param x is a vector
#'
#' @return a logical value
#'
#' @examples
#' unique.all(c("A","A","B"))
#'
#' @export

unique.all <- function(x) {length(x)==length(unique(na.omit(x)))}