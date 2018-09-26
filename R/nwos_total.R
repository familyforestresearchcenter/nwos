#' NWOS Total
#'
#' Estimators used to calculate NWOS total.
#' @usage nwos_total(weight, area = 1, domain = 1, variable = 1)
#' @param weight vector of weights per ownership.
#' @param area vector of area (e.g., forest acres) per ownership. Default = 1 (i.e., estimates are in terms of ownerships).
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param variable vector of variable of interest. Default = 1 (i.e., variable is ignored).
#' @keywords nwos
#' @details
#' If area is set to 1, then the esitmates are in terms of ownerships.
#' Variable can be binary or continuous. If variable is binary, the value returned is the number of ownerships or acreage in the domain of interest with that attribute. If variable is continuous, the value returned is the variable total in terms of ownerships or acreage.
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' WI_FFO_OWN_TOTAL <- nwos_total(weight = wi$WEIGHT)

nwos_total <- function(weight, area = 1, domain = 1, variable = 1) {
  sum(weight * area * domain * variable, na.rm=T)
}
