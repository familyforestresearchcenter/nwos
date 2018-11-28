#' NWOS Replicate Response Rates
#'
#' This function calculates response rates for NWOS replicates and is designed to be used with an apply function, such as sapply.
#' @usage nwos_response_rate(index, data, stratum.name, response.name)
#' @param index list of observations (i.e., replicates) in data to include.
#' @param data data frame containing stratum variable.
#' @param stratum.name the name of the variable in data indicating inclusion (1) and exclusion (0) in the stratum of interest
#' @param response.name name of the variable in data indicating response (1) and non-response (0).
#' @details
#' This function needs to be run by state.
#' @return
#' Response rate in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- wi %>% mutate(RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0))
#' nwos_response_rate_replicates(index = WI_REPLICATES[[1]], data = wi, stratum.name = "FFO", response.name = "RESPONSE")

# nwos_response_rate <- function(stratum, point.count, response)
nwos_response_rate_apply <- function(index, stratum, point.count, response) {
  nwos_response_rate(stratum[index], point.count[index], response[index])
}

