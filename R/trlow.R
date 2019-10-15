#' trlow
#'
#' Trims and sets to lowercase
#'
#' @param x is a character vector
#'
#' @return an lowercase character vector with leading and trailing spaces removed
#'
#' @examples
#' trlow(" foo ")
#'
#' @export

trlow <- function(x) {tolower(trim(x))}