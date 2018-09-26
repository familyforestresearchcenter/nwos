#' NWOS State LandUse Ownership Domain Variable Wrapepr function
#'
#' This function estimates the stratum area.
#' @usage nwos_stratum_area(stratum, point.count, state.area)
#' @param df data frame containing, at a minimum, the following variables: STATE_CD, LAND_USE, OWN_CD, and POINT_COUNT
#' @param state.list vector of state codes to be evaluated.
#' @param state.area vector of total land area in state. It needs to have the same length as state.list and be in the
#' same order.
#' @param land.use.list vector of land uses to be evaluated. The defaualt value is 1 (forest).
#' @param own.cd.list vector of ownership codes to be evaluated. The default value if 45 (family).
#' @details
#' @return data frame of areas in state/land use/ownership strata
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' ADD EXAMPLE HERE

nwos_estimate_replicates <- function(r,
                                     state.area,
                                     point.count, response,
                                     owner.area, stratum, domain,
                                     replicates, index,
                                     stat = "TOTAL",
                                     units = "OWNERSHIPS") {
  stratum.r <- stratum[replicates[,r]]
  point.count.r <- point.count[replicates[,r]]
  response.r <- response[replicates[,r]]
  owner.area.r <- owner.area[replicates[,r]]
  domain.r <- domain[replicates[,r]]

  STRATUM_AREA_REP <- nwos_stratum_area(state.area = state.area,
                                        stratum = stratum.r,
                                        point.count = rep(1, NROW(replicates[,r])))
  STRATUM_RESPONSE_RATE <- nwos_response_rate(point.count = point.count.r[stratum.r %in% 1],
                                              response = response.r[stratum.r %in% 1])
  WEIGHT <- nwos_weights(stratum.area = STRATUM_AREA_REP,
                         response.rate = STRATUM_RESPONSE_RATE,
                         point.count = point.count.r,
                         owner.area = owner.area.r,
                         stratum = stratum.r)
  if(stat == "TOTAL" & units == "OWNERSHIPS") x <- nwos_total(weight = WEIGHT, domain = domain.r)
  if(stat == "TOTAL" & units == "ACRES") x <- nwos_total(weight = WEIGHT, area = owner.area.r, domain = domain.r)
  return(x)
}
