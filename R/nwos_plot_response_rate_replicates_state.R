#' NWOS Plot Response Rates for Replicates by State
#'
#' This function calculates response rates for NWOS replicates and is designed to be used with an apply function, such as sapply.
#' @usage nwos_response_rate_replicates_state(state, data = po, s.name = "FFO", r.name = "RESPONSE")
#' @param state
#' @param data = po
#' @param s.name = "FFO"
#' @param r.name = "RESPONSE"
#' @return
#' Response rate in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0), RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0), RESPONSE = if_else(is.na(RESPONSE_PROPENSITY), 0, RESPONSE))
#' WI_FFO_RR_REP <- sapply(WI_REPLICATES, nwos_response_rate_apply, index = wi$ROW_NAME, stratum = wi$FFO, response = wi$RESPONSE)
#' WI_FFO_RR_REP

nwos_plot_response_rate_replicates_state <- function(state, data = po, s.name = "FFO", r.name = "RESPONSE") {
  sapply(REPLICATES[[state]],
         nwos_plot_response_rate_replicates,
         index = data %>% filter(STATECD_NWOS %in% state) %>% pull(PLOT_OWNER_CN_INTEGER),
         stratum = data %>% filter(STATECD_NWOS %in% state) %>% pull(!!s.name),
         response = data %>% filter(STATECD_NWOS %in% state) %>% pull(!!r.name))
}
