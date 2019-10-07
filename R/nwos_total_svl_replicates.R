#' NWOS Total by State, Variable, and Level for Replicates
#'
#' Estimator used to calculate NWOS totals by state, variable and level.
#' @usage nwos_total_svl(states, variables, levels, data = QUEST, data.area = NA, data.weights = "WEIGHT")
#' @param states = "1"
#' @param variables = "HOME"
#' @param variables
#' @param levels = "1",
#' @param data = QUEST_WIDE
#' @param data.area = NA
#' @param data.weights = "WEIGHT"
#' @keywords nwos
#' @details
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' ??

nwos_total_svl_replicates <- function (r = 1, rep = REPLICATES,
                                       states = "1", variables = "HOME", levels = "1",
                                       data = QUEST_WIDE, data.area = NA, data.weight = "WEIGHT",
                                       data.stratum = NA, data.domain = NA)
{
  rep.r <- rep %>% filter(REP %in% r)
  data.rep.r <- (rep.r %>% select(CN = RESPONSE_CN, FINAL_WEIGHT = WEIGHT, PLOT_COUNT)) %>%
    left_join((data %>% select(-FINAL_WEIGHT, -PLOT_COUNT)), by = "CN") %>%
    mutate(WEIGHT = FINAL_WEIGHT * PLOT_COUNT)
  mapply(nwos_total_svl, states, variables, levels,
         MoreArgs = list(data = data.rep.r, data.area = data.area, data.weight = data.weight,
                         data.stratum = data.stratum, data.domain = data.domain))
}
