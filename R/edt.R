#' edt
#'
#' Converts MS Excel DATETIME to Date
#'
#' @param x is a character vector of in MS Excel DATETIME format
#'
#' @return a date vector
#'
#' @examples
#' edt(43383)
#'
#' @export

edt <- function(x){as.Date(as.numeric(x),origin='1899-12-30')}
