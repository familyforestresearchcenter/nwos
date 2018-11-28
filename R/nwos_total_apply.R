#' NWOS Total Apply
#'
#' A version of the nwos_total function that is intended to be used with an apply function, typically for estimating sampling errors.
#' @usage nwos_total_appl(index, weight, area, domain, variable)
#' @param replicate an atomic value indicating which replicate to use.
#' @param index list of vectors indicating which data to include. There needs to be an element with the same name as the replicate being assessed.
#' @param weight list of vectors of weights. There needs to be an element with the same name as the replicate being assessed.
#' @param area vector of area (e.g., forest acres) per ownership.
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise.
#' @param variable vector of variable of interest.
#' @keywords nwos
#' @details
#' If index is set to 1:length(x), the results are equivalent to nwos_total.
#' Default values for area, domain, and variable are not set, as they are for nwos_total, in order to reduce computation time.
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' NEED TO ADD

nwos_total_apply <- function(r, index, weight, area, domain, variable) {
  index <- unlist(index[r])
  nwos_total(weight = unlist(weight[r]), area = area[index],
             domain = domain[index], variable = variable[index])
}
