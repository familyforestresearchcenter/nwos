#' NWOS Total
#'
#' Estimators used to calculate NWOS totals and, optionally, associated variances
#' @usage nwosTotal(weight, area=NA, point.count=NA, response=NA, domain=1, stratum.area=1, units="ownerships", variance=T, R=1000)
#' @param weight vector of weights per ownership.
#' @param area vector of area (e.g., forest acres) per ownership.
#' @param point.count vector of counts of sample points per ownership/record.
#' @param response vector with 1 indicating response and 0 nonresponse.
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise.
#' @param stratum.area area for stratum (e.g., acres of family forestland in a state).
#' @param units units of analysis. Permissable values are "ownerships" or "area". Default is "ownerships".
#' @param variance logic variable indicating if variances should be calculated
#' @param R=1000
#' @keywords nwos
#' @details
#' Variances are calculated using a bootstrapping approach.
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' data <- NWOS_DATA_SAMPLE[NWOS_DATA_SAMPLE$SAMPLE==1,]
#' data$WEIGHT <- nwosWeights(data$POINT_COUNT, data$ACRES_FOREST, 1000)
#' data$DOMAIN <- 1
#' nwosTotal(data$WEIGHT, var=F)
#' nwosTotal(weight=data$WEIGHT, point.count=data$POINT_COUNT, response=data$RESPONSE, area=data$ACRES_FOREST,
#' domain=data$DOMAIN, stratum.area=1000)
#' nwosTotal(data$WEIGHT, data$ACRES_FOREST, variance=F, units="area")
#' nwosTotal(weight=data$WEIGHT, point.count=data$POINT_COUNT, response=data$RESPONSE, area=data$ACRES_FOREST,
#' domain=data$DOMAIN, stratum.area=1000, units="area")

# nwos_total <- function(stratum, domain=1, weight, area=NA, units="ownerships"){
#   if(!units %in% c("ownerships","area")) stop("units need to be ownerships or area")
#   if(units=="ownerships")
#     x <- sum(stratum * domain * weight, na.rm=T) # Ownerships estimator
#   else
#     x <- sum(stratum * domain * weight * area, na.rm=T) # Area estimator
#   return(x)
# }

# nwos_total <- function(weight, area=NA, domain=1, variable=1, units="OWNERSHIPS"){
#   if(!units %in% c("OWNERSHIPS","AREA")) stop("units need to be OWNERSHIPS or AREA")
#   if(units=="OWNERSHIPS")
#     x <- sum(weight * domain * variable, na.rm=T) # Ownerships estimator
#   else
#     x <- sum(weight * area * domain * variable, na.rm=T) # Area estimator
#   return(x)
# }

nwos_total <- function(weight, area = 1, domain = 1, variable = 1){
  sum(weight * area * domain * variable, na.rm=T)
}
