#' NWOS Total by State, Variable, and Level
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

nwos_total_svl <- function(states = "1", variables = "OWNTYPE", levels = "1",
                           data = QUEST_WIDE, data.area = NA, data.weights = "WEIGHT",
                           data.stratum = NA, data.domain = NA) {
  weight <- data[[data.weights]]
  area <- if(is.na(data.area)) 1 else data[[data.area]] # AC_WOOD
  state <- ifelse(data[["STATECD_NWOS"]] %in% states, 1, 0)
  stratum <- if(is.na(data.stratum)) 1 else data[[data.stratum]]
  domain <- if(is.na(data.domain)) 1 else data[[data.domain]]
  variable <- ifelse(data[[variables]] %in% levels, 1, 0)
  nwos_total(weight = weight, area = area, stratum = state * stratum,
             domain = domain, variable = variable)
}

