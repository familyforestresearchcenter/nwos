#' trup
#'
#' Trims and sets to uppercase
#'
#' @param x is a character vector
#'
#' @return an uppercase character vector with leading and trailing spaces removed
#'
#' @examples
#' trup(" foo ")
#'
#' @export

trup <- function(x) {toupper(trim(x))}