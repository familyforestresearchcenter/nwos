#' NWOS Proportion
#'
#' This function calculates proportions for the NWOS.
#' @usage nwos_proportion(weight, area = 1, domain, base = 1, variable = 1)
#' @param weight vector of weights per ownership.
#' @param area vector of area (e.g., forest acres) per ownership. Default = 1 (i.e., estimates are in terms of ownerships).
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param base vector with 1 indicating inclusion in the base (i.e., denominator) and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param variable vector of binary variable of interest. 1 = Yes and 0 = No.
#' @keywords nwos
#' @details
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' nwos_proportion(weight = wi$WEIGHT, domain = wi$Y_1)
#' nwos_proportion(weight = wi$WEIGHT, area = wi$AC_WOOD, domain = wi$Y_1)

nwos_proportion <- function(weight, area = 1, domain = 1, variable = 1)
{
  nwos_total(weight = weight, area = area, domain = domain, variable = variable) /
    nwos_total(weight = weight, area = area, domain = domain, variable = 1)
}
