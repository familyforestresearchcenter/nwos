#' NWOS Response Rate
#'
#' This function calculates response rates for the NWOS.
#' @usage nwos_response_rate(stratum, point.count, response)
#' @param stratum vector indicating inclusion (1) and exclusion (0) in the stratum of interest.
#' @param point.count vector of the number of sample points associated with each observation.
#' @param response vector indicating response (1) and non-response (0).
#' @details
#' This function needs to be run by state.
#' @return
#' Response rate in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- wi %>% mutate(RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0))
#' WI_FFO_RR <- nwos_response_rate(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE)

nwos_response_rate <- function(stratum, point.count, response) {
  data <- data.frame(stratum = stratum, point.count = point.count, response = response) # Create data frame
  n.s <- data %>% filter(stratum %in% c(1), response %in% c(0,1)) %>% summarize(sum(point.count)) # Number of sample points in stratum
  n.s.r <- data %>% filter(stratum %in% c(1), response %in% c(1)) %>% summarize(sum(point.count)) # Number of respondent sample points in stratum
  as.numeric(n.s.r / n.s) # Calculate response rate
}
