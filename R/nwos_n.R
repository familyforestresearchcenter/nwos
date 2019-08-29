#' NWOS n
#'
#' Count number of responses (n).
#' @usage nwos_n(point.count = 1, stratum = 1, domain = 1)
#' @param point.count vector of number of sample points associated with each observation. Default = 1.
#' @param stratum vector with 1 indicating inclusion in the stratum and 0 otherwise. Default = 1 (i.e., all ownerships are in the same stratum).
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param variable vector of variable of interest. Default = 1 (i.e., variable is ignored).
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.

nwos_n <- function(point.count = 1, stratum = 1, domain = 1) {
  sum(point.count * stratum * domain, na.rm=T)
}
