#' NWOS Weights
#'
#' This function returns the calculated weights based on the NWOS sample design.
#' @usage nwosWeights(point.count, area, domain, stratum.area, response.rate)
#' @param point.count vector of number of sample points. Needs to sum to total number of sample points across all land uses and ownership classes.
#' @param area vector of areas of forest land owned by sampled ownerships. NAs are permissable for ownerships not in owner.class.
#' @param area.total total land area (e.g., total land area in the state).
#' @param response.rate response rate for the stratum. Default is 1 (i.e., 100% response).
#' @param stratum vector indicating whether ownership is in the stratum. Default is 1 (i.e., all points are in the same stratum).
#' @return vector of weights
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' df <- nwos.data.sample[nwos.data.sample$SAMPLE==1,]
#' w <- nwosWeights(point.count=df$POINT_COUNT, area=df$ACRES_FOREST, area.state=1000)
#' sum(w)

nwosWeights <- function(point.count, area, area.total, response.rate=1, stratum=NA)
{
  if(is.na(stratum)) stratum <- rep(1,NROW(point.count))
  n <- sum(point.count) # Sample size
  n.stratum <- sum(point.count[stratum==1])
  area.stratum <- (n.stratum/n) * area.state

  w <- ifelse(stratum==1, (area.stratum / (area * point.count * n.stratum)) * (1 / response.rate), NA)

  return(w)
}
