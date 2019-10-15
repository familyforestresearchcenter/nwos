#' trim
#'
#' Trims leading and trailing whitespace
#'
#' @param x is a character vector
#'
#' @return a character vector with leading and trailing spaces removed
#'
#' @examples
#' trim(" foo ")
#'
#' @export

trim <- function(x) {gsub("^\\s+|\\s+$", "", x)}