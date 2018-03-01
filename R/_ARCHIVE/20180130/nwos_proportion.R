#' NWOS Proportion
#'
#' This function calculates totals for the NWOS.
#' @param weight
#' @param point.count number of sample points on the land of ownership i in stratum s.
#' @param domain
#' @param y
#' @param area area of forest land owned by ownership i in stratum s.
#' @param units . Defauly is "ownerships".
#' @return vector of weights.
#' @keywords nwos
#' @export
#' @examples
#' nwosProportion(weight, point.count, domain, y, area, units="ownerships")

nwosProportion <- function(weight, point.count, domain, y, area, units="ownerships")
{
  if(units=="ownerships")
    x.mean <- sum(weight*point.count*domain*y, na.rm=T) /sum(weight*point.count*domain, na.rm=T)
  if(units=="area")
    x.mean <- sum(weight*point.count*domain*y*area, na.rm=T) / sum(weight*point.count*domain*area, na.rm=T)
  return(x.mean)
}
