#' NWOS Weights for Replicates by State
#'
#' This function returns the calculated weights based on the NWOS sample design and is designed to be used with an apply function, such as lapply.
#' @usage nwos_weights_replicates(r, index.rep, index, stratum, response, area, stratum.area, stratum.area.correction = stratum.area, response.rate)
#' @return
#' list of vector of weights
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' ??

nwos_weights_replicates_state <- function(state, data = po, s.name = "FFO", r.name = "RESPONSE") {
  sapply(1:length(REPLICATES[[state]]),
         nwos_weights_replicates,
         index.rep = REPLICATES[[state]],
         index = data %>% filter(STATECD_NWOS %in% state) %>% pull(PLOT_OWNER_CN_INTEGER),
         stratum = data %>% filter(STATECD_NWOS %in% state) %>% pull(!!s.name),
         response = data %>% filter(STATECD_NWOS %in% state) %>% pull(!!r.name),
         area = data %>% filter(STATECD_NWOS %in% state) %>% pull(AC_WOOD),
         stratum.area = ffo.acres.rep[[state]],
         response.rate = ffo.rr.rep[[state]])
}
