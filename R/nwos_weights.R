#' NWOS Weights
#'
#' This function returns the calculated weights based on the NWOS sample design.
#' @usage nwos_weights(stratum, point.count, response, owner.area, stratum.area, stratum.area.correction = stratum.area, response.rate)
#' @param stratum vector indicating inclusion (1) and exclusion (0) in the stratum of interest.
#' @param point.count vector of the number of sample points associated with each observation.
#' @param response vector indicating response (1) and non-response (0).
#' @param owner.area vector of the area (of forestland) owned by each ownership.
#' @param stratum.area area (of forestland) in the stratum of interest.
#' @param stratum.area.correction area (of forestland) in the stratum of interest to which the total stratum.area will be forced. The default value is stratum.area (i.e., no correction).
#' @param response.rate response rate for the stratum of interest. Value should range from 0 ot 1.
#' @details
#' @return vector of weights
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi$WEIGHT <- nwos_weights(stratum = wi$FFO, point.count = wi$POINT_COUNT,
#' response = wi$RESPONSE, owner.area = wi$AC_WOOD,
#' stratum.area = WI_FFO_AREA, response.rate = WI_FFO_RR)

nwos_weights <- function(stratum, point.count, response, owner.area,
                         stratum.area, stratum.area.correction = stratum.area, response.rate) {
  data <- data.frame(stratum = stratum, point.count = point.count, response = response, owner.area = owner.area) # Create data frame
  n.s <- as.numeric(data %>% filter(stratum %in% c(1), response %in% c(0,1)) %>% summarize(sum(point.count))) # Number of sample points in stratum
  stratum.area <- stratum.area.correction # Corrected stratum area
  ifelse(data$owner.area == 0, 0,
         ((stratum.area / (data$owner.area * n.s)) * data$point.count) * (1 / response.rate) * data$stratum * data$response) # Weights
}
