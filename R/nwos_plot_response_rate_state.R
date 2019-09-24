#' NWOS Plot Response Rate by State
#'
#' This function calculates response rates for the NWOS by state.
#' @usage nwos_plot_response_rate(stratum, point.count, response)
#' @param state = "1"
#' @param data = po
#' @param s.name = "FFO"
#' @param r.name = "RESPONSE"
#' @return
#' Response rate in the stratum calculated by state.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' ??

nwos_plot_response_rate_state <- function(state = "1", data = po, s.name = "FFO", r.name = "RESPONSE") {
  nwos_plot_response_rate(stratum = data %>% filter(STATECD_NWOS %in% state) %>% pull(!!s.name),
                           response = data %>% filter(STATECD_NWOS %in% state) %>% pull(!!r.name))
}
