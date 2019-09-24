#' NWOS Make Replicates
#'
#' Generate replicates that can be used for NWOS bootstrapping variance estimation.
#' @usage nwos_make_replicates(index, point.count, R = 2500)
#' @param index vector of names (e.g., row names) used to identify observations.
#' @param point.count the number of sample points associated with each observation. Default is 1.
#' @param R number of replicates or bootstraps. Default is 2500.
#' @return
#' List containing indices for the observations that make up each of the replicates.
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @keywords nwos
#' @export
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0))
#' WI_REPLICATES <- nwos_replicates(index = row.names(wi), point.count = wi$POINT_COUNT, R = 100)

nwos_make_replicates <- function(index, point.count = 1, R = 2500) {
  index <- rep(index, point.count)
  if(length(point.count) == 1) point.count <- rep(1, length(index))
  replicates <- replicate(R, sample(index, length(index), replace = T)) # Generate replicates
  split(replicates, col(replicates)) # Convert to list
}
