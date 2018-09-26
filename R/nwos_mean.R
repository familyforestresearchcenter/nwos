#' NWOS Mean
#'
#' This function calculates means for the NWOS.
#' @usage nwos_mean(weight, area = 1, domain = 1, base = 1, variable)
#' @param weight vector of weights per ownership.
#' @param area vector of area (e.g., forest acres) per ownership. Default = 1 (i.e., estimates are in terms of ownerships).
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param variable vector of variable of interest.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' nwos_mean(weight = wi$WEIGHT, variable = wi$Y_3)
#' nwos_mean(weight = wi$WEIGHT, area = wi$AC_WOOD, variable = wi$Y_3)

nwos_mean <- function(weight, area = 1, domain = 1, base = 1, variable)
{
  nwos_total(weight = weight, area = area, domain = domain, variable = variable) /
    nwos_total(weight = weight, area = area, domain = domain, variable = 1)
}
