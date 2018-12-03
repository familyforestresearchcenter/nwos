#' NWOS Mean
#'
#' Calculate means for the NWOS.
#' @usage nwos_mean(weight, area = 1, domain = 1, variable)
#' @param weight vector of weights.
#' @param area vector of area (e.g., forest acres in the stratum). Default = 1 (i.e., estimates are in terms of ownerships).
#' @param domain binary vector (i.e., dummay variable) with 1 indicating inclusion in the domain  of interest and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param variable vector of values for the variable of interest.
#' @return Mean of variable of interest.
#' @keywords nwos
#' @details
#' This function needs to be run by stratum (e.g., family forest ownerships in a state).
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' nwos_mean(weight = wi$WEIGHT, variable = wi$Y_3)
#' nwos_mean(weight = wi$WEIGHT, area = wi$AC_WOOD, variable = wi$Y_3)

nwos_mean <- function(weight, area = 1, domain = 1, variable)
{
  nwos_total(weight = weight, area = area, domain = domain, variable = variable) /
    nwos_total(weight = weight, area = area, domain = domain, variable = 1)
}
