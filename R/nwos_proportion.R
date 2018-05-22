#' NWOS Proportion
#'
#' This function calculates totals for the NWOS.
#' @usage nwosProportion(weight, point.count, domain, y, area, units="ownerships")
#' @param weight weight for each observation.
#' @param y variable of interest. 1 indicates inclusion.
#' @param domain variable indicating whether ownership is in the domain of interest.
#' @param area vector of areas of forest land owned by sampled ownerships. NAs are permissable.
#' @param units units of analysis. Permissable values are "ownerships" or "area". Default is "ownerships".
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' df <- nwos.data.sample[nwos.data.sample$SAMPLE==1,]
#' df$WEIGHT <- nwosWeights(point.count=df$POINT_COUNT, area=df$ACRES_FOREST, area.total=1000)
#' # Proportion of ownerships with Y_1=1
#' nwosProportion(weight=df$WEIGHT, y=df$Y_1)
#' # Proportion of acres with Y_1=1
#' nwosProportion(weight=df$WEIGHT, y=df$Y_1, area=df$ACRES_FOREST, units="area")

nwosProportion <- function(weight, y=1, stratum=1, domain=1, area=NA, units="ownerships")
{
  if(units=="ownerships")
    x.prop <- sum(weight * domain * y, na.rm=T) /sum(weight * domain, na.rm=T)
  if(units=="area")
    x.prop <- sum(weight * domain * y * area, na.rm=T) / sum(weight * domain * area, na.rm=T)
  return(x.prop)
}
