#' NWOS Proportion
#'
#' This function calculates proportions for the NWOS.
#' @usage nwos_proportion(weight, area = 1, stratum = 1, domain = 1, base = 1, variable)
#' @param weight vector of weights per ownership.
#' @param area vector of area (e.g., forest acres) per ownership. Default = 1 (i.e., estimates are in terms of ownerships).
#' @param stratum vector with 1 indicating inclusion in the stratum and 0 otherwise. Default = 1 (i.e., all ownerships are in the same stratum).
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param variable vector of binary variable of interest. 1 = Yes and 0 = No.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0), RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0), RESPONSE = if_else(is.na(RESPONSE_PROPENSITY), 0, RESPONSE))
#' WI_FFO_AREA <- nwos_stratum_area(stratum = wi$FFO, point.count = wi$POINT_COUNT, state.area = 33898733)
#' WI_FFO_RR <- nwos_response_rate(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE)
#' wi$WEIGHT <- nwos_weights(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE, area = wi$AC_WOOD, stratum.area = WI_FFO_AREA, response.rate = WI_FFO_RR)
#' WI_FFO_OWN_Y1_PROP <- nwos_proportion(weight = wi$WEIGHT, variable = wi$Y_1)
#' WI_FFO_OWN_Y1_PROP
#' WI_FFO_AC_Y1_PROP <- nwos_proportion(weight = wi$WEIGHT, area = wi$AC_WOOD, variable = wi$Y_1)
#' WI_FFO_AC_Y1_PROP

nwos_proportion <- function(weight, area = 1, stratum = 1, domain = 1, variable)
{
  nwos_total(weight = weight, area = area, stratum = stratum, domain = domain, variable = variable) /
    nwos_total(weight = weight, area = area, stratum = stratum, domain = domain, variable = 1)
}
