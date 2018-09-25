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


# nwos_response_rate <- function(index = NA, data, stratum = "FFO", point.count = "POINT_COUNT", response = "RESPONSE") {
#   if(is.na(index[1])) index <- row.names(data)
#   if(point.count == 1) x <- data.frame(data[index, stratum], 1, data[index, response])
#   else x <- data.frame(data[index, c(stratum, point.count, response)]) # Create data frame
#   names(x) <- c("stratum", "point.count", "response")
#   n.s <- x %>% filter(stratum %in% c(1), response %in% c(0,1)) %>% summarize(sum(point.count)) # Number of sample points in stratum
#   n.s.r <- x %>% filter(stratum %in% c(1), response %in% c(1)) %>% summarize(sum(point.count)) # Number of respondent sample points in stratum
#   as.numeric(n.s.r / n.s) # Calculate response rate
# }


nwos_weights <- function(x = R2,
                         data = wi,
                         stratum.area,
                         stratum.area.correction=stratum.area,
                         stratum = "FFO", point.count = "POINT_COUNT", response = "RESPONSE",
                         # response.rate,
                         # point.count,
                         owner.area = "ACRES",
                         # stratum = 1
                         ) {
  stratum.area <- x[[1]]$AREA
  response.rate <- x[[1]]$RR
  index <- unlist(x[[1]]$INDEX)
  if(is.na(index[1])) index <- row.names(data)
  # if(point.count == 1) x <- data.frame(data[index, stratum], 1, data[index, response])
  # else
  x2 <- data.frame(data[index, c(stratum, point.count, response, owner.area)]) # Create data frame
  names(x2) <- c("stratum", "point.count", "response", "owner.area")
  n.s <- as.numeric(x2 %>% filter(stratum %in% c(1), response %in% c(0,1)) %>% summarize(sum(point.count))) # Number of sample points in stratum
  stratum.area <- stratum.area.correction # Corrected stratum area

  ((stratum.area / (x2$owner.area * n.s)) * x2$point.count) * (1 / response.rate) * x2$stratum # Weights

  #   n.s.r <- x %>% filter(stratum %in% c(1), response %in% c(1)) %>% summarize(sum(point.count)) # Number of respondent sample points in stratum
  #   as.numeric(n.s.r / n.s) # Calculate response rate

  # n.s <- sum(point.count * stratum) # Sample points in stratum
  # stratum.area <- stratum.area.correction # Corrected stratum area
  # ((stratum.area / (owner.area * n.s)) * point.count) * (1 / response.rate) * stratum # Weights
}

# nwos_weights <- function(stratum.area,
#                          stratum.area.correction=stratum.area,
#                          response.rate,
#                          point.count,
#                          owner.area,
#                          stratum = 1) {
#   n.s <- sum(point.count * stratum) # Sample points in stratum
#   stratum.area <- stratum.area.correction # Corrected stratum area
#   ((stratum.area / (owner.area * n.s)) * point.count) * (1 / response.rate) * stratum # Weights
# }
