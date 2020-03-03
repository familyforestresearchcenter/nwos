#' NWOS Weights for Replicates
#'
#' This function returns the calculated weights based on the NWOS sample design and is designed to be used with an apply function, such as lapply.
#' @usage nwos_weights_replicates(r, index.rep, index, stratum, response, area, stratum.area, stratum.area.correction = stratum.area, response.rate)
#' @param r vector of replicates numbers.
#' @param index.rep list of observations (i.e., replicates) to include.
#' @param index vector used to identify the location of values in the other vectors (e.g., row names).
#' @param stratum vector indicating inclusion (1) and exclusion (0) in the stratum of interest. NA's are allowed.
#' @param response vector indicating response (1) and non-response (0).
#' @param area vector of the area (of forestland) owned by each ownership.
#' @param stratum.area vector of area (of forestland) in the stratum of interest for each replicate.
#' @param stratum.area.correction vector of area (of forestland) in the stratum of interest to which the total stratum.area will be forced. The default value is stratum.area (i.e., no correction).
#' @param response.rate vector of response rates for each replicate for the stratum of interest.
#' @return
#' list of vector of weights
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

nwos_weights_replicates <- function(r, index.rep, index, stratum, response, area,
                                    stratum.area, stratum.area.correction = stratum.area, response.rate) {
  index.rep <- unlist(index.rep[r])
  nwos_weights(stratum = stratum[match(index.rep, index)],
               response = response[match(index.rep, index)],
               area = area[match(index.rep, index)],
               stratum.area = stratum.area[r],
               response.rate = response.rate[r])
}
