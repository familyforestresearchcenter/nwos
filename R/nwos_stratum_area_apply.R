#' NWOS Replicate Stratum Areas
#'
#' This function estimates NWOS stratum areas for NWOS replicates and is designed to be used with an apply function, such as sapply.
#' @usage nwos_stratum_area_apply(index.rep, index, stratum, state.area)
#' @param index.rep list of observations (i.e., replicates) to include.
#' @param index vector used to identify the location of values in the other vectors (e.g., row names).
#' @param stratum vector indicating inclusion (1) and exclusion (0) in the stratum of interest.
#' @param state.area total land area in the state of interest.
#' @details
#' point.count in nwos_stratum_area is  set to 1 since the replicates are an expanded list (one observation per sample point).
#' @return
#' Area of forestland in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0))
#' WI_REPLICATES <- nwos_replicates(index = row.names(wi), point.count = wi$POINT_COUNT, R = 100)
#' WI_FFO_AREA_REP <- sapply(WI_REPLICATES, nwos_stratum_area_apply, index = wi$ROW_NAME, stratum = wi$FFO, state.area = 33898733)
#' WI_FFO_AREA_REP

nwos_stratum_area_apply <- function(index.rep, index, stratum, state.area) {
  index.rep <- unlist(index.rep)
  nwos_stratum_area(stratum = stratum[match(index.rep, index)],
                    state.area = state.area)
}
