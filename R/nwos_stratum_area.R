#' NWOS Stratum Area
#'
#' This function estimates NWOS stratum areas.
#' @usage nwos_stratum_area(stratum, point.count, state.area)
#' @param stratum vector indicating inclusion (1) and exclusion (0) in the stratum of interest. NA's are allowed.
#' @param point.count vector of the number of sample points associated with each observation. Default is point.count = 1.
#' @param state.area total land area in the state of interest.
#' @return
#' Area (of forestland) in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0))
#' WI_FFO_AREA <- nwos_stratum_area(stratum = wi$FFO, point.count = wi$POINT_COUNT, state.area = 33898733)
#' WI_FFO_AREA

nwos_stratum_area <- function(stratum, point.count = 1, state.area) {
  if(length(point.count) == 1) point.count <- rep(1, length(stratum))
  n <- sum(point.count[stratum %in% c(0,1)]) # Number of sample points
  n.s <- sum(point.count[stratum %in% c(1)]) # Number of sample points in stratum
  (n.s / n) * state.area # Calculate stratum area
}
