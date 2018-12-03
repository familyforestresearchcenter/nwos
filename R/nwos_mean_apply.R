#' NWOS Proportion
#'
#' This function calculates means for NWOS replicates. This is typically used with an apply function.
#' @usage nwos_proportion_replicates(r, index.rep, data, weight.rep, owner.area.name = "OWNER", domain.name, base.name = "FFO", variable.name = "FFO")
#' @param r vector of replicates. Typically 1:length(index).
#' @param index list of rows to include in each replicate.
#' @param weight vector of weights.
#' @param area vector of area (e.g., forest acres in the stratum).
#' @param domain binary vector (i.e., dummay variable) with 1 indicating inclusion in the domain of interest and 0 otherwise.
#' @param variable vector of values for the variable of interest.
#' @return Mean of variable of interest.
#'  @keywords nwos
#' @seealso nwos_mean
#' @export
#' @details
#' This function needs to be run by stratum (e.g., family forest ownerships in a state).
#' Due to indexing (to allow for apply function), there are fewer defauly values than nwos_mean.
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples

nwos_mean_apply <- function(r, index, weight, area, domain, variable) {
  index <- unlist(index[r])
  nwos_total(weight = unlist(weight[r]), area = area[index], domain = domain[index], variable = variable[index]) /
    nwos_total(weight = unlist(weight[r]), area = area[index], domain = domain[index], variable = 1)
}
