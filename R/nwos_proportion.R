#' NWOS Proportion
#'
#' This function calculates totals for the NWOS.
#' @usage nwosProportion(weight, point.count, domain, y, area, units="ownerships")
#' @param weight weight for each observation.
#' @param point.count vector of number of sample points. Needs to sum to total number of sample points across all land uses and ownership classes.
#' @param domain variable indicating whether ownership is in the domain of interest.
#' @param y variable of interest. Set to 1 if interested in basic ownership or area totals.
#' @param area vector of areas of forest land owned by sampled ownerships. NAs are permissable.
#' @param units units of analysis. Permissable values are "ownerships" or "area". Default is "ownerships".
#' @keywords nwos
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_sample_data.RData")
#' nwos.sample.data$domain <- ifelse(nwos.sample.data$owner.class=="FamilyForest", 1, 0)
#' nwos.sample.data$response <- ifelse(nwos.sample.data$owner.class=="FamilyForest",
#' as.numeric(as.character(nwos.sample.data$response)), NA)
#' nwos.sample.data$weights <- nwosWeights(point.count=nwos.sample.data$point.count,
#' area=nwos.sample.data$area,
#' domain=nwos.sample.data$domain,
#' stratum.area=35198019,
#' response.rate=sample.response.rate)
#' # Proportion of ownership with y_1=1
#' nwos.sample.data$y_1 <- as.numeric(as.character(nwos.sample.data$y_1))
#' nwosProportion(weight=nwos.sample.data$weight,
#' point.count=nwos.sample.data$point.count,
#' domain=nwos.sample.data$domain,
#' y=nwos.sample.data$y_1,
#' area=nwos.sample.data$area,
#' units="ownerships")
#' # Proportion of area with y_1=1
#' nwosProportion(weight=nwos.sample.data$weight,
#' point.count=nwos.sample.data$point.count,
#' domain=nwos.sample.data$domain,
#' y=nwos.sample.data$y_1,
#' area=nwos.sample.data$area,
#' units="area")

nwosProportion <- function(weight, point.count, domain, y, area, units="ownerships")
{
  if(units=="ownerships")
    x.mean <- sum(weight*point.count*domain*y, na.rm=T) /sum(weight*point.count*domain, na.rm=T)
  if(units=="area")
    x.mean <- sum(weight*point.count*domain*y*area, na.rm=T) / sum(weight*point.count*domain*area, na.rm=T)
  return(x.mean)
}
