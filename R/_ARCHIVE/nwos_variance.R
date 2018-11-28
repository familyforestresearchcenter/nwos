#' NWOS Variance
#'
#' This function calculates variances for NWOS statistics using a bootstrapping approach.
#' @usage nwosVar(state.area, point.count, response, owner.area, stratum, domain, variable = 1,
#' replicates, index, stat = "TOTAL", units = "OWNERSHIPS")
#' @param units units of analysis. Permissable values are "OWNERSHIPS" or "AREA". Default is "OWNERSHIPS".
#' @param stat statistic of interest. Permissable values are "TOTAL", "mean" or "proportion". Default is "total".
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @keywords nwos
#' @export
#' @examples
#' NEED TO ADD

nwos_variance <- function(state.area,
                          point.count, response, owner.area,
                          stratum, domain, variable = 1,
                          replicates, index,
                          stat = "TOTAL", units = "OWNERSHIPS") {
  var(sapply(1:NCOL(replicates), nwos_estimate_replicates,
             state.area = state.area,
             point.count = point.count, response = response, owner.area = owner.area,
             stratum = stratum, domain = domain,
             replicates = replicates, index = index,
             stat = stat, units = units))
}
