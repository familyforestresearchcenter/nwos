#' nullif
#'
#' Set null values to empty strings
#'
#' @param x is a vector with null and nonnull values
#'
#' @return a character vector with null values replaced by an empty string
#'
#' @examples
#' nullif(c(1,2,NA))
#'
#' @export

nullif <- function(x){x[is.na(x)] <- ""
                      return(x)}
