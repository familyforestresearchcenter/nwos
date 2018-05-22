#' NWOS Stratum Area
#'
#' This function estimates the area by state and stratum.
#' @usage nwosWeights(point.count, area, domain, stratum.area, response.rates)
#' @param data data frame of survey data. See details below for required variables.
#' @param state.areas data frame of state areas. See details below for required variables.
#' @param response.rates data frame of survey data. See details below for required variables.
#' @details
#' @return vector of weights
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' data <- NWOS_DATA_SAMPLE[NWOS_DATA_SAMPLE$SAMPLE==1,]
#' state.areas <- data.frame(STATE=0,AREA=1000)
#' response.rates <- nwosResponseRate(data)
#' data <- nwosWeights(data, state.areas, response.rates)

# nwosWeights <- function(point.count, stratum, area, stratum.area, response.rate)
# {
#   n.s <- sum(data.i.j$POINT_COUNT, na.rm=T)  # Sample size in stratum j in state i
#   area.i.j <- (n.s/n) * state.area
#
#   data.i.j$WEIGHT <- ((area.i.j / (data.i.j$AREA * n.i.j)) * data.i.j$POINT_COUNT) * (1 / response.rate)
#   data.out <- rbind(data.out, data.i.j)
# }
# }
#
# var.list <- c("STATE", "STRATUM", "OWN_ID", "AREA", "POINT_COUNT", "RESPONSE", "WEIGHT")
# data.out <- data.out[,c(var.list,names(data.out[!names(data.out)%in%var.list]))]
#
# return(data.out)
# }
