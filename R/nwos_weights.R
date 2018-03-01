#' NWOS Weights
#'
#' This function returns the calculated weights based on the NWOS sample design.
#' @usage nwosWeights(point.count, area, domain, stratum.area, response.rate)
#' @param point.count vector of number of sample points. Needs to sum to total number of sample points across all land uses and ownership classes.
#' @param area vector of areas of forest land owned by sampled ownerships. NAs are permissable for ownerships not in owner.class.
#' @param owner.class vector of owner classes.
#' @param stratum.area total area of land.
#' @param response.rate vector of response rates.
#' @param response.rate.owner.class vector of owner.class associated with each response.rate
#' @return vector of weights
#' @keywords nwos
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' # Calculate weights for family forest ownerships
#' load("data/nwos_sample_data.RData")
#' nwos.sample.data$domain <- ifelse(nwos.sample.data$owner.class=="FamilyForest", 1, 0)
#' nwos.sample.data$response <- ifelse(nwos.sample.data$owner.class=="FamilyForest",
#' as.numeric(as.character(nwos.sample.data$response)), NA)
#' sample.response.rate <- nwosResponseRates(nwos.sample.data$point.count,nwos.sample.data$response)
#' nwos.sample.data$weights <- nwosWeights(point.count=nwos.sample.data$point.count,
#' area=nwos.sample.data$area,
#' domain=nwos.sample.data$domain,
#' stratum.area=35198019,
#' response.rate=sample.response.rate)

nwosWeights <- function(point.count, area, domain, stratum.area, response.rate)
{
  n <- sum(point.count) # Sample size

  w <- ifelse(domain==1, (stratum.area / (n * area)) * (1 / response.rate), NA)

  return(w)
}
