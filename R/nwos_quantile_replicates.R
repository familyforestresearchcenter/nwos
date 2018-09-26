#' NWOS Quantile
#'
#' This function calculates quantiles for NWOS estimates for replicates. This is typically used with an apply function.
#' @usage nwos_quantile(weight, area = 1, domain, variable, prob=c(0.00, 0.25, 0.50, 0.75, 1.00), max.iter=1000)
#' @param weight vector of weights per ownership.
#' @param area vector of area (e.g., forest acres) per ownership. Default = 1 (i.e., estimates are in terms of ownerships).
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param variable vector of variable of interest in terms of the quantile.
#' @param prob vector of probabilities for quantiles. Defualt is 0.00, 0.25, 0.50, 0.75, 1.00 (i.e., quartiles).
#' @param max.iter maximum number of iterations. Default = 1000.
#' @keywords nwos
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' nwos_quantile_replicates(weight = wi$WEIGHT, domain = wi$FFO, variable = wi$AC_WOOD)
#' WI_FFO_OWN_QUANT_REP <- sapply(1:2, # length(WI_REPLICATES),
#' nwos_quantile_replicates,
#' index.rep = WI_REPLICATES, data = wi,
#' weight.rep = WI_FFO_WEIGHTS_REP, owner.area.name = "OWNER",
#' domain.name = "FFO", variable.name = "AC_WOOD")


nwos_quantile_replicates <- function(r, index.rep, data,
                                     weight.rep,
                                     owner.area.name = "OWNER",
                                     domain.name = "FFO",
                                     variable.name = "FFO",
                                     prob=c(0.00, 0.25, 0.50, 0.75, 1.00), max.iter=1000)
{

  data <- data.frame(weight = weight.rep[[r]],
                     area = unlist(data[index.rep[[r]], owner.area.name]),
                     domain = unlist(data[index.rep[[r]], domain.name]),
                     variable = unlist(data[index.rep[[r]], variable.name])) # Create data frame
  nwos_quantile(weight = data$weight, area = data$area, domain = data$domain, variable = data$variable,
                            prob = prob, max.iter = max.iter)
}
