#' NWOS Response Rate
#'
#' This function calculates response rate for the NWOS.
#' @usage nwos_response_rate(point.count, response)
#' @param df data frame containing, at a minimum, the following variables: STATE_CD, LAND_USE, OWN_CD, POINT_COUNT, and RESPONSE
#' @param state.list vector of state codes to be evaluated.
#' @param land.use.list vector of land uses to be evaluated. The defaualt value is 1 (forest).
#' @param own.cd.list vector of ownership codes to be evaluated. The default value if 45 (family).
#' @details
#' @return data frame of response rates by state/land use/ownership strata
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' data <- NWOS_DATA_SAMPLE[NWOS_DATA_SAMPLE$SAMPLE==1,]
#' nwos_response_rate(data$POINT_COUNT, data$RESPONSE)

# nwos_response_rate <- function(point.count, response)
# {
#   rr <- sum(point.count[response%in%c(1)]) / sum(point.count[response%in%c(1,0)]) # Calcuate respons rate
#   return(rr) # Return response rate
# }

# nwos_response_rate <- function(stratum, point.count, response) {
#   n.s <- sum(point.count[stratum %in% c(1) & response %in% c(0,1)]) # Number of sample points in stratum
#   n.s.r <- sum(point.count[stratum %in% c(1) & response %in% c(1)]) # Number of sample points in stratum for respondents
#   n.s.r / n.s  # Respons rate
# }

nwos_response_rate <- function(point.count, response) {
  n.s <- sum(point.count[response %in% c(0,1)]) # Number of sample points in stratum
  n.s.r <- sum(point.count[response %in% c(1)]) # Number of sample points in stratum for respondents
  n.s.r / n.s  # Respons rate
}
