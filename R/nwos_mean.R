#' NWOS Mean
#'
#' This function calculates totals for the NWOS.
#' @usage nwosMean(weight, point.count, domain, y=1, area, units="ownerships")
#' @param weight weight for each observation.
#' @param y variable of interest. Set to 1 if interested in basic ownership or area totals. Default is 1.
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
#' # Mean acreage by ownership
#' nwosMean(weight=df$WEIGHT, y=df$ACRES_FOREST)
#' # Mean acreage by area
#' nwosMean(weight=df$WEIGHT, y=df$ACRES_FOREST, area=df$ACRES_FOREST, units="area")

nwosMean <- function(weight, y=1, stratum=1, domain=1, area=NA, units="ownerships")
{
  if(units=="ownerships")
    x.mean <- sum(weight * domain * y, na.rm=T) /sum(weight * domain, na.rm=T)
  if(units=="area")
    x.mean <- sum(weight * domain * y * area, na.rm=T) / sum(weight * domain * area, na.rm=T)
  return(x.mean)
}
