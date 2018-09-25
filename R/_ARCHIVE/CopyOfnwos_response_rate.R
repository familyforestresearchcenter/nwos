#' NWOS Response Rate
#'
#' This function calculates response rates for the NWOS.
#' @usage nwos_response_rate(index = NA, data, stratum = "FFO", point.count = "POINT_COUNT", response = "RESPONSE")
#' @param index vector of observations in data to include. If NA, the index is set to the row names of data (i.e., uses all of the rows in data)
#' @param data data frame containing stratum and point.count variables.
#' @param stratum the name of a variable in data indicating inclusion (1) and exclusion (0) in the stratum of interest.
#' @param point.count name of a variable in data indicating the number of sample points associated with each observation.
#' @param response name of the variable in data indicating response (1) and non-response (0).
#' @details
#' This function needs to be run by state.
#' @return
#' Response rate in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- wi %>% mutate(FFO = if_else(LAND_USE == 1 & OWN_CD == 45, 1, 0), RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0))
#' WI_FFO_RR <- nwos_response_rate(data = wi)

nwos_response_rate <- function(index = NA, data, stratum = "FFO", point.count = "POINT_COUNT", response = "RESPONSE") {
  if(is.na(index[1])) index <- row.names(data)
  if(point.count == 1) x <- data.frame(data[index, stratum], 1, data[index, response])
  else x <- data.frame(data[index, c(stratum, point.count, response)]) # Create data frame
  names(x) <- c("stratum", "point.count", "response")
  n.s <- x %>% filter(stratum %in% c(1), response %in% c(0,1)) %>% summarize(sum(point.count)) # Number of sample points in stratum
  n.s.r <- x %>% filter(stratum %in% c(1), response %in% c(1)) %>% summarize(sum(point.count)) # Number of respondent sample points in stratum
  as.numeric(n.s.r / n.s) # Calculate response rate
}
