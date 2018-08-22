#' NWOS Stratum Area
#'
#' This function estimates the stratum area.
#' @usage nwos_stratum_area(stratum, point.count, state.area)
#' @param df data frame containing, at a minimum, the following variables: STATE_CD, LAND_USE, OWN_CD, and POINT_COUNT
#' @param state.list vector of state codes to be evaluated.
#' @param state.area vector of total land area in state. It needs to have the same length as state.list and be in the
#' same order.
#' @param land.use.list vector of land uses to be evaluated. The defaualt value is 1 (forest).
#' @param own.cd.list vector of ownership codes to be evaluated. The default value if 45 (family).
#' @details
#' @return data frame of areas in state/land use/ownership strata
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' data <- NWOS_DATA_SAMPLE[NWOS_DATA_SAMPLE$SAMPLE==1,]
#' nwos_stratum_area()

nwos_stratum_area <- function(state.area, stratum, point.count) {
  n <- sum(point.count[stratum %in% c(0,1)], na.rm=T) # Sample size (number of points) in state
  n.s <- sum(point.count[stratum %in% c(1)], na.rm=T) # Sample size (number of points) in state stratum
  (n.s / n) * state.area # Calculate stratum area
}
