#' NWOS Total Apply
#'
#' A version of the nwos_total function that is intended to be used with an apply function, typically for estimating sampling errors.
#' @usage nwos_total_apply(r, index.rep, index, weight, area = 1, domain = 1, variable = 1)
#' @param r vector of replicates numbers.
#' @param index.rep list of observations (i.e., replicates) to include.
#' @param index vector used to identify the location of values in the other vectors (e.g., row names).
#' @param weight list of weights for each observation in each replicate.
#' @param area vector of the area (of forestland) for each observation. Default = 1.
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise. Default = 1.
#' @param variable vector of variable of interest. Default = 1.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0), RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0), RESPONSE = if_else(is.na(RESPONSE_PROPENSITY), 0, RESPONSE))
#' WI_REPLICATES <- nwos_replicates(index = row.names(wi), point.count = wi$POINT_COUNT, R = 100)
#' WI_FFO_AREA_REP <- sapply(WI_REPLICATES, nwos_stratum_area_apply, index = wi$ROW_NAME, stratum = wi$FFO, state.area = 33898733)
#' WI_FFO_RR_REP <- sapply(WI_REPLICATES, nwos_response_rate_apply, index = wi$ROW_NAME, stratum = wi$FFO, response = wi$RESPONSE)
#' WI_FFO_WEIGHTS_REP <- lapply(1:length(WI_REPLICATES), nwos_weights_apply,index.rep = WI_REPLICATES, index = wi$ROW_NAME, stratum = wi$FFO, response = wi$RESPONSE, area = wi$AC_WOOD,stratum.area = WI_FFO_AREA_REP, response.rate = WI_FFO_RR_REP)
#' WI_FFO_OWN_TOTAL_REP <- sapply(1:length(WI_REPLICATES), nwos_total_apply, index.rep = WI_REPLICATES, index = wi$ROW_NAME, weight = WI_FFO_WEIGHTS_REP)
#' WI_FFO_OWN_TOTAL_REP

nwos_total_apply <- function(r, index.rep, index, weight, area = 1, domain = 1, variable = 1) {
  index.rep <- unlist(index.rep[r])
  if(length(area) != 1) area <- area[match(index.rep, index)]
  if(length(domain) != 1) domain <- domain[match(index.rep, index)]
  if(length(variable) != 1) variable <- variable[match(index.rep, index)]
  nwos_total(weight = unlist(weight[r]), area = area, domain = domain, variable = variable)
}
