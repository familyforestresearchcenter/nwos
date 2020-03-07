#' NWOS Estimates Total
#'
#' Estimator used to calculate NWOS totals.
#' @usage nwos_total(weight, area = 1, stratum = 1, domain = 1, variable = 1)
#' @param weight vector of weights per observation.
#' @param area vector of area (e.g., forest acres) per observation. Default = 1 (i.e., estimates are in terms of ownerships).
#' @param stratum vector with 1 indicating inclusion in the stratum and 0 otherwise. Default = 1 (i.e., all ownerships are in the same stratum).
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param variable vector of variable of interest. Default = 1 (i.e., variable is ignored).
#' @keywords nwos
#' @details
#' If area is set to 1, then the esitmates are in terms of ownerships.
#' Variable can be binary or continuous. If variable is binary, the value returned is the number of ownerships or acreage in the domain of interest with that attribute. If variable is continuous, the value returned is the variable total in terms of ownerships or acreage.
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0), RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0), RESPONSE = if_else(is.na(RESPONSE_PROPENSITY), 0, RESPONSE))
#' WI_FFO_AREA <- nwos_stratum_area(stratum = wi$FFO, point.count = wi$POINT_COUNT, state.area = 33898733)
#' WI_FFO_RR <- nwos_response_rate(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE)
#' wi$WEIGHT <- nwos_weights(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE, area = wi$AC_WOOD, stratum.area = WI_FFO_AREA, response.rate = WI_FFO_RR)
#' WI_FFO_OWN_TOTAL <- nwos_total(weight = wi$WEIGHT)
#' WI_FFO_OWN_TOTAL
#' WI_FFO_AC_TOTAL <- nwos_total(weight = wi$WEIGHT, area = wi$AC_WOOD)
#' WI_FFO_AC_TOTAL

nwos_estimates_total <- function(weight, area = 1, stratum = 1, domain = 1, variable = 1) {
  sum(weight * area * stratum * domain * variable, na.rm=T)
}
