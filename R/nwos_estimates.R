#' NWOS Estimates Total
#'
#' Estimator used to calculate NWOS totals by state, variable and level.
#' @usage nwos_total_svl(state, variable, level, data = QUEST, area = NA, weights = "WEIGHT")
#' @param state = "1"
#' @param variable = "HOME"
#' @param variable
#' @param level = "1",
#' @param data = QUEST_WIDE
#' @param area = NA
#' @param weights = "WEIGHT"
#' @keywords nwos
#' @details
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' state = "1"
#' variable = "OWNTYPE"
#' level = "1"
#' data = QUEST[[1]]
#' area = NA
#' weights = "WEIGHT"
#' stratum = NA
#' domain = NA
#' ??

nwos_estimates <- function(imp = 1:5,
                           geo = tibble(GEO_ABB = "AL", GEO_CD = "1"),
                           reps = NA, replicates = REPLICATES_LIST,
                           variables = tibble(VARIABLE = "TOTAL", LEVEL = 1),
                           data = QUEST, area = NA, weights = "WEIGHT",
                           stratum = "FFO", domain = "ONEPLUS") {
  n.cores <- detectCores() - 1
  nwos.estimates.rep <- mclapply(as.character(0:5), # as.character(0:2500)
                                 nwos_estimates_rep,
                                 variable.list = VARIABLE_LIST[1:5,],
                                 geo.list = GEO_LIST[1:2,],
                                 mc.cores = n.cores)

  nwos.estimates.summary <- nwos_esitmates_summary(nwos_estimates_rep)

  return(nwos.estimates.summary)
}
