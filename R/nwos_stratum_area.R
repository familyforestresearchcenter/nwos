#' NWOS Stratum Area
#'
#' This function estimates NWOS stratum areas.
#' @usage nwos_stratum_area(stratum, point.count, state.area)
#' @param stratum vector indicating inclusion (1) and exclusion (0) in the stratum of interest.
#' @param point.count vector of the number of sample points associated with each observation.
#' @param state.area total land area in the state of interest.
#' @details
#' This function needs to be run by state.
#' @return
#' Area (of forestland) in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- wi %>% mutate(FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0))
#' WI_FFO_AREA <- nwos_stratum_area(stratum = wi$FFO, point.count = wi$POINT_COUNT, state.area = 33898733)
#' WI_FFO_AREA

nwos_stratum_area <- function(stratum, point.count, state.area) {
  data <- data.frame(stratum = stratum, point.count = point.count)
  n <- data %>% filter(stratum %in% c(0,1)) %>% summarize(sum(point.count)) # Number of sample points
  n.s <- data %>% filter(stratum %in% c(1)) %>% summarize(sum(point.count)) # Number of sample points in stratum
  as.numeric((n.s / n) * state.area) # Calculate stratum area
}
