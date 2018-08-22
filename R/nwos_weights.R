#' NWOS Weights
#'
#' This function returns the calculated weights based on the NWOS sample design.
#' @usage nwosWeights(point.count, area, domain, stratum.area, response.rates)
#' @param point.count vector of counts of sample points per ownership.
#' @param area vector of area (e.g., forest acres) per ownership.
#' @param stratum.area area for stratum
#' @param response.rate response rate for stratum. Default value = 1.
#' @details
#' @return vector of weights
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' data <- NWOS_DATA_SAMPLE[NWOS_DATA_SAMPLE$SAMPLE==1,]
#' data$w <- nwosWeights(data$POINT_COUNT, data$ACRES_FOREST, 1000)

nwos_weights <- function(stratum.area,
                         stratum.area.correction=stratum.area,
                         response.rate,
                         point.count,
                         owner.area,
                         stratum = 1) {
  n.s <- sum(point.count * stratum) # Sample points in stratum
  stratum.area <- stratum.area.correction # Corrected stratum area
  ((stratum.area / (owner.area * n.s)) * point.count) * (1 / response.rate) * stratum # Weights
}
