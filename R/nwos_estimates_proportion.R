#' NWOS Estimate Mean
#'
#' Calculate means for the NWOS.
#' @usage nwos_estimates_mean(weight, area = 1, domain = 1, variable)
#' @param weight vector of weights.
#' @param area vector of area (e.g., forest acres in the stratum). Default = 1 (i.e., estimates are in terms of ownerships).
#' @param stratum vector with 1 indicating inclusion in the stratum and 0 otherwise. Default = 1 (i.e., all ownerships are in the same stratum).
#' @param domain binary vector (i.e., dummay variable) with 1 indicating inclusion in the domain  of interest and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param variable vector of values for the variable of interest.
#' @return Mean of variable of interest.
#' @keywords nwos
#' @details
#' This function needs to be run by stratum (e.g., family forest ownerships in a state).
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0), RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0), RESPONSE = if_else(is.na(RESPONSE_PROPENSITY), 0, RESPONSE))
#' WI_FFO_AREA <- nwos_stratum_area(stratum = wi$FFO, point.count = wi$POINT_COUNT, state.area = 33898733)
#' WI_FFO_RR <- nwos_response_rate(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE)
#' wi$WEIGHT <- nwos_weights(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE, area = wi$AC_WOOD, stratum.area = WI_FFO_AREA, response.rate = WI_FFO_RR)
#' nwos_mean(weight = wi$WEIGHT, variable = wi$Y_3)
#' nwos_mean(weight = wi$WEIGHT, area = wi$AC_WOOD, variable = wi$Y_3)

nwos_estimates_proportion <- function(weight, area = 1, stratum = 1, domain = 1, variable, level = "1",
                                na.rm = T, na.value = -1:-3)
{
  if(na.rm) variable <- ifelse(variable %in% na.value, NA, as.character(variable))
  variable <- ifelse(is.na(variable), NA,
                     ifelse(variable %in% level, 1, 0))
  nwos_estimates_total(weight = weight, area = area, stratum = stratum, domain = domain, variable = variable) /
    nwos_estimates_total(weight = weight, area = area, stratum = stratum, domain = domain, variable = 1)
}
