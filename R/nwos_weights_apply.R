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

# nwos_weights <- function(stratum, point.count, response, area,
#                          stratum.area, stratum.area.correction = stratum.area, response.rate)
nwos_weights_apply <- function(r, index.rep, index, stratum, point.count, response, area,
                               stratum.area, stratum.area.correction = stratum.area, response.rate) {
  index.rep <- unlist(index.rep[r])
  nwos_weights(stratum = stratum[match(index.rep, index)],
               response = response[match(index.rep, index)],
               area = area[match(index.rep, index)],
               stratum.area = stratum.area[r],
               response.rate = response.rate[r])
}
