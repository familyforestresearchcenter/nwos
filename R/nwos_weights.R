#' NWOS Weights
#'
#' This function returns the calculated weights based on the NWOS sample design.
#' @usage nwos_weights(stratum, point.count, response, area, stratum.area, stratum.area.correction = stratum.area, response.rate)
#' @param stratum vector indicating inclusion (1) and exclusion (0) in the stratum of interest.
#' @param point.count vector of the number of sample points associated with each observation. Default point.count = 1.
#' @param response vector indicating response (1) and non-response (0).
#' @param area vector of the area (of forestland) owned by each ownership.
#' @param stratum.area area (of forestland) in the stratum of interest.
#' @param stratum.area.correction area (of forestland) in the stratum of interest to which the total stratum.area will be forced. The default value is stratum.area (i.e., no correction).
#' @param response.rate response rate for the stratum of interest. Value should range from 0 ot 1.
#' @return
#' vector of weights
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0), RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0), RESPONSE = if_else(is.na(RESPONSE_PROPENSITY), 0, RESPONSE))
#' WI_FFO_AREA <- nwos_stratum_area(stratum = wi$FFO, point.count = wi$POINT_COUNT, state.area = 33898733)
#' WI_FFO_RR <- nwos_response_rate(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE)
#' wi$WEIGHT <- nwos_weights(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE, area = wi$AC_WOOD, stratum.area = WI_FFO_AREA, response.rate = WI_FFO_RR)
#' wi$WEIGHT

nwos_weights <- function(stratum, point.count = 1, response, area,
                         stratum.area, stratum.area.correction = stratum.area, response.rate) {
  if(length(point.count) == 1) point.count <- rep(1, length(stratum))
  n.s <- sum(point.count[stratum %in% c(1) & response %in% c(0,1)]) # Number of sample points in stratum
  stratum.area <- stratum.area.correction # Corrected stratum area
  ifelse(area == 0, 0,
         ((stratum.area / (area * n.s)) * point.count) * (1 / response.rate) * stratum * response) # Weights
}
