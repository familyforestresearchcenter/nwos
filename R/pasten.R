#' pasten
#'
#' Paste function where nulls equal ""
#'
#' @param x is a series of string vectors
#'
#' @return a string vector
#'
#' @examples
#' pasten('an unbroken',NA,'string')
#'
#' @export

pasten <- function(...,sep=" "){do.call(paste,c(lapply(list(...),nullif), sep=sep))}

