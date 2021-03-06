#' NWOS Replicates by State
#'
#' Generate replicates by state that can be used for NWOS bootstrapping variance estimation.
#' @usage nwos_replicates_state(x, data = po)
#' @return
#' ??
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @keywords nwos
#' @export
#' @examples
#' ??

nwos_make_replicates_state <- function(x, data = po, R = 10) {
  ID <- data %>% filter(STATECD_NWOS %in% x) %>% pull(PLOT_OWNER_CN_INTEGER)
  nwos_make_replicates(ID, R = R)
}
