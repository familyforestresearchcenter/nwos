#' NWOS Replicate Weights
#'
#' This function returns the calculated weights based on the NWOS sample design and is designed to be used with an apply function, such as sapply.
#' @usage nwos_weights_replicates(r, index.rep, data, stratum.name = "FFO", point.count.name = "POINT_COUNT",
#' response.name = "RESPONSE", owner.area.name = "AC_WOOD", stratum.area.rep, response.rate.rep)
#' @param r vector of replicates. See details below.
#' @param index.rep list of observations (i.e., replicates) in data to include.
#' @param data data frame containing stratum variable.
#' @param stratum.name name of the variable in data indicating inclusion (1) and exclusion (0) in the stratum of interest. Default = "FFO".
#' @param point.count.name name of a variable in data of the number of sample points associated with each observation.. Default = "POINT_COUNT",
#' @param response.name name of a variable in data indicating response (1) and non-response (0). Default = "RESPONSE".
#' @param owner.area.name nae of a variable in data of the area (of forestland) owned by each ownership. Default = "AC_WOOD".
#' @param stratum.area.rep vector of area (of forestland) in the stratum of interest for each replicate.
#' @param response.rate.rep vector of response rates for each replicate for the stratum of interest.
#' @details
#' r it typcially set using 1:length(WI_REPLICATES)
#' index.rep differs from index in other nwos functions in that these includes multiple lists (i.e., all of the replicates, not just one).
#' index.rep, stratum.area.rep, and response.rate.rep are assumed to be listed in the same order.
#' @return vector of weights
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' WI_FFO_WEIGHTS_REP <- lapply(1:length(WI_REPLICATES), nwos_weights_replicates,
#' index.rep = WI_REPLICATES, data = wi,
#' stratum.area.rep = WI_FFO_AREA_REP, response.rate.rep = WI_FFO_RR_REP)

nwos_weights_replicates <- function(r, index.rep, data,
                                    stratum.name = "FFO",
                                    point.count.name = "POINT_COUNT",
                                    response.name = "RESPONSE",
                                    owner.area.name = "AC_WOOD",
                                    stratum.area.rep, response.rate.rep) {
  # index.r <- index[[r]]
  data <- data.frame(stratum = unlist(data[index.rep[[r]], stratum.name]),
                     point.count = 1,
                     response = unlist(data[index.rep[[r]], response.name]),
                     owner.area = unlist(data[index.rep[[r]], owner.area.name])) # Create data frame
  nwos_weights(stratum = data$stratum,
               point.count = data$point.count,
               response = data$response,
               owner.area = data$owner.area,
               stratum.area = stratum.area.rep[r],
               response.rate = response.rate.rep[r])
  # n.s <- as.numeric(data %>% filter(stratum %in% c(1), response %in% c(0,1)) %>% summarize(sum(point.count))) # Number of sample points in stratum
  # stratum.area <- stratum.area.correction # Corrected stratum area
  # ifelse(data$owner.area == 0, 0,
  #        ((stratum.area / (data$owner.area * n.s)) * data$point.count) * (1 / response.rate) * data$stratum) # Weights
}

# nwos_response_rate_replicates <- function(index, data, stratum.name, response.name) {
#   data <- data.frame(stratum = unlist(data[index, stratum.name]), point.count = 1, response = unlist(data[index, response.name])) # Create data frame
#   nwos_response_rate(data$stratum, data$point.count, data$response)
# }

# nwos_weights <- function(stratum, point.count, response, owner.area,
#                          stratum.area, stratum.area.correction = stratum.area, response.rate) {
#   data <- data.frame(stratum = stratum, point.count = point.count, response = response, owner.area = owner.area) # Create data frame
#   n.s <- as.numeric(data %>% filter(stratum %in% c(1), response %in% c(0,1)) %>% summarize(sum(point.count))) # Number of sample points in stratum
#   stratum.area <- stratum.area.correction # Corrected stratum area
#   ifelse(data$owner.area == 0, 0,
#          ((stratum.area / (data$owner.area * n.s)) * data$point.count) * (1 / response.rate) * data$stratum) # Weights
# }


# replicate.list # index, stratum.area, stratum.area.correction, response.rate
#
#
#
# x <- list(index = WI_REPLICATES[[1]],
#                        stratum.area = WI_FFO_AREA_REP[1],
#                        response.rate = WI_FFO_RR_REP[1])
#
# temp.func <- function(x) {WI_FFO_AREA_REP[x] * WI_FFO_RR_REP[x]}
#
# temp.func <- function(x) {str(x)}
#
#
# sapply(1:2, temp.func)
#
# # stratum.area, stratum.area.correction = stratum.area, response.rate
#
#
