#' NWOS Weights by State
#'
#' This function returns the calculated weights based on the NWOS sample design.
#' @usage nwos_weights(stratum, point.count, response, area, stratum.area, stratum.area.correction = stratum.area, response.rate)
#' @param state state
#' @param data = po
#' @return
#' vector of weights
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' ??

nwos_weights_state <- function(state, data = po) {
  bind_cols(PLOT_OWNER_CN = data %>% filter(STATECD_NWOS %in% state) %>% pull(PLOT_OWNER_CN),
            WEIGHT = nwos_weights(stratum = data %>% filter(STATECD_NWOS %in% state) %>% pull(FFO),
                                  response = data %>% filter(STATECD_NWOS %in% state) %>% pull(RESPONSE),
                                  area = data %>% filter(STATECD_NWOS %in% state) %>% pull(AC_WOOD),
                                  nonresponse.adj = data %>% filter(STATECD_NWOS %in% state) %>% pull(WEIGHT_ADJ_NONRESPONSE),
                                  stratum.area = ffo.acres %>% filter(STATECD_NWOS %in% c(state)) %>% pull(ACRES),
                                  stratum.area.correction = fia.fa %>% filter(STATECD_NWOS == state, OWNGRP == 'Family') %>% pull(ACRES),
                                  response.rate = ffo.rr %>% filter(STATECD_NWOS %in% c(state)) %>% pull(RESPONSE_RATE)))
}
