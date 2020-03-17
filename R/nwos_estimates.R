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
#' quest.list = QUEST_LIST
#' variable.list = VARIABLE_LIST
#' geo.list = GEO_LIST
#' rep = "0"
#' rep.list = REPLICATE_LIST
#' stratum = "FFO"
#' domain = "ONEPLUS"
#' start.time <- Sys.time()
#' x <- nwos_estimates(geo.list = GEO_LIST[1,], rep = (0:100))
#' Sys.time() - start.time

nwos_estimates <- function(quest.list = QUEST_LIST,
                           variable.list = VARIABLE_LIST,
                           geo.list = GEO_LIST,
                           rep = (0:2500),
                           rep.list = REPLICATE_LIST,
                           stratum = "FFO",
                           domain = "ONEPLUS") {
  n.cores <- detectCores() - 1
  nwos.estimates.rep <- mclapply(as.character(rep),
                                 nwos_estimates_rep,
                                 variable.list = variable.list,
                                 geo.list = geo.list,
                                 stratum = stratum,
                                 domain = domain,
                                 mc.cores = n.cores)

  # saveRDS(nwos.estimates.rep, "DATA/NWOS_ESTIMATES_REP.RDS")

  # nwos.estimates.summary <- nwos_esitmates_summary(nwos.estimates.rep) %>%
  # mutate(STRATUM = stratum, DOMAIN = domain)

  # saveRDS(nwos.estimates.rep, "DATA/NWOS_ESTIMATES_REP.RDS")

  return(nwos.estimates.rep)
  # return(nwos.estimates.summary)
}
