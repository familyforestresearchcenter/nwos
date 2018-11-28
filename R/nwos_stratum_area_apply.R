#' NWOS Replicate Stratum Areas
#'
#' This function estimates NWOS stratum areas for NWOS replicates and is designed to be used with an apply function, such as sapply.
#' @usage nwos_stratum_area_replicates(index, data, stratum.name, state.area)
#' @param index list of observations (i.e., replicates) in data to include.
#' @param data data frame containing stratum variable.
#' @param stratum.name the name of the variable in data indicating inclusion (1) and exclusion (0) in the stratum of interest
#' @param state.area total land area in the state of interest.
#' @param point.count the number of sample points associated with each observation.
#' @details
#' This function needs to be run by state.
#' point.count is automatically set to 1 since the replicates are an expanded list (one observation per sample point).
#' @return
#' Area of forestland in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- wi %>% mutate(FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0)) # Add stratum variable, FFO
#' WI_FFO_AREA_REP <- sapply(WI_REPLICATES, nwos_stratum_area_replicates, data = wi, stratum.name = "FFO", state.area = 33898733)

# nwos_stratum_area <- function(stratum, point.count, state.area)
nwos_stratum_area_apply <- function(index, stratum, point.count, state.area) {
  nwos_stratum_area(stratum = stratum[index], point.count = point.count[index], state.area = state.area)
}
