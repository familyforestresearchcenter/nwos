#' NWOS Weights
#'
#' @description nwosWeights returns the calculated weights based on the NWOS sample design.
#' @usage nwosWeights <- function(point.count, area, owner.class, stratum.area, response.rate, response.rate.owner.class)
#' @param point.count vector of number of sample points. Needs to sum to total number of sample points across all land uses and ownership classes.
#' @param area vector of areas of forest land owned by sampled ownerships. NAs are permissable for ownerships not in owner.class.
#' @param owner.class vector of owner classes.
#' @param stratum.area total area of land.
#' @param response.rate vector of response rates.
#' @param response.rate.owner.class vector of owner.class associated with each response.rate
#' @details
#' Lengths and orders of point.count, area, and owner.class need to match.
#' Units (e.g., acres) need to match between area and stratum.area.
#' response.rate needs to be a proportion.
#' Lengths and orders of response.rate and response.rate.owner.class need to match.
#' @return vector of weights.
#' @keywords nwos
#' @export
#' @examples
#' load("ANALYSIS/R/nwos/data/nwos_data_20180107.RData")
#' nwos.response.rate <- nwosResponseRates(nwos.data)
#' nwos.data.weights <-
#'     nwosWeights(nwos.data,
#'     stratum.area=data.frame(stratum="WI", area=35198019),
#'     response.rate=nwos.response.rate)

nwosWeights <- function(point.count, area, domain, stratum.area, response.rate)
{
  n <- sum(point.count) # Sample size

  w <- ifelse(domain==1, (stratum.area / (n * area)) * (1 / response.rate), NA)

  return(w)
}
