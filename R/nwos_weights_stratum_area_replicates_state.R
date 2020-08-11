#' NWOS Stratum Areas for Replicates by State
#'
#' This function estimates NWOS stratum areas for NWOS replicates and is designed to be used with an apply function, such as sapply.
#' @usage nwos_stratum_area_apply(index.rep, index, stratum, state.area)
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

nwos_weights_stratum_area_replicates_state <- function(state, data = po, s.name = "FFO") {
  sapply(REPLICATES[[state]],
         nwos_stratum_area_replicates,
         index = data %>% filter(STATECD_NWOS %in% state) %>% pull(PLOT_OWNER_CN_INTEGER),
         stratum = data %>% filter(STATECD_NWOS %in% state) %>% pull(FFO),
         state.area = land.area %>% filter(STATECD_NWOS %in% state) %>% pull(ACRES))
}
