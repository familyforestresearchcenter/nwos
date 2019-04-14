#' NWOS Response Rate
#'
#' This function calculates response rates for the NWOS.
#' @usage nwos_response_rate(stratum, point.count, response)
#' @param stratum vector indicating inclusion (1) and exclusion (0) in the stratum of interest.
#' @param point.count vector of the number of sample points associated with each observation. Default is 1.
#' @param response vector indicating response (1) and non-response (0).
#' @return
#' Response rate in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0), RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0), RESPONSE = if_else(is.na(RESPONSE_PROPENSITY), 0, RESPONSE))
#' WI_FFO_RR <- nwos_response_rate(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE)
#' WI_FFO_RR

nwos_response_rate <- function(stratum, point.count = 1, response) {
  if(length(point.count) == 1) point.count <- rep(1, length(stratum))
  n.s <- sum(point.count[stratum %in% c(1) & response %in% c(0,1)]) # Number of sample points in stratum
  n.s.r <- sum(point.count[stratum %in% c(1) & response %in% c(1)]) # Number of respondent sample points in stratum
  n.s.r / n.s # Calculate response rate
}
