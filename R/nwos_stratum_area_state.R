#' NWOS Stratum Area by State
#'
#' This function estimates NWOS stratum areas by state.
#' @usage nwos_stratum_area_state(state, data = po, s.name = "FFO", data.area = land.area, ac.name = "ACRES")
#' @param state
#' @param data = po
#' @param s.name = "FFO"
#' @param data.area = land.area
#' @param ac.name = "ACRES"
#' @return
#' Area (of forestland) in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' ??

nwos_stratum_area_state <- function(state, data = po, s.name = "FFO", data.area = land.area, ac.name = "ACRES") {
  nwos_stratum_area(stratum = data %>% filter(STATECD_NWOS %in%state) %>% pull(!!s.name),
                    state.area = land.area %>% filter(STATECD_NWOS %in% state) %>% pull(!!ac.name))
}
