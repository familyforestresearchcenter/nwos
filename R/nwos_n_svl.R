#' NWOS Sample Sizes by State, Variable, and Level
#'
#' Calculate NWOS sample sizes by state, variable and level.
#' @usage nwos_n_svl(states, variables, levels, data = QUEST_WIDE)
#' @param states = "1"
#' @param variables = "HOME"
#' @param levels = "1",
#' @param data = QUEST_WIDE
#' @keywords nwos
#' @details
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' ??

nwos_n_svl <- function(s = "1", v = "HOME", l = "1", data = QUEST_WIDE) {
  nwos_n(stratum = ifelse(data$STATECD_NWOS %in% s, 1, 0),
         domain = ifelse(data[[v]] %in% l, 1, 0))
}
