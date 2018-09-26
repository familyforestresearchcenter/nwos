#' NWOS Proportion
#'
#' This function calculates means for the NWOS replicates. This is typically used with an apply function.
#' @usage nwos_proportion_replicates(r, index.rep, data, weight.rep, owner.area.name = "OWNER", domain.name, base.name = "FFO", variable.name = "FFO")
#' @param r vector of replicates. See details below.
#' @param index.rep list of observations (i.e., replicates) in data to include.
#' @param data data frame containing stratum variable.
#' @param weight.rep list with each element the weights for the observations in that replicate.
#' @param owner.area.name nae of a variable in data of the area (of forestland) owned by each ownership. Default = "OWNER" (i.e., 1, which is used for estimating number of ownerships).
#' @param domain.name name of the variable in data indicating inclusion (1) and exclusion (0) in the domain of interest. Default = "FFO".
#' @param base.name name of a variable in data with 1 indicating inclusion in the base (i.e., denominator) and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param variable.name name of the variable of interest in data.
#' @keywords nwos
#' @details
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples

nwos_mean_replicates <- function(r, index.rep, data,
                                       weight.rep,
                                       owner.area.name = "OWNER",
                                       domain.name = "FFO",
                                       variable.name)
{
  data <- data.frame(weight = weight.rep[[r]],
                     area = unlist(data[index.rep[[r]], owner.area.name]),
                     domain = unlist(data[index.rep[[r]], domain.name]),
                     variable = unlist(data[index.rep[[r]], variable.name])) # Create data frame
  nwos_total(weight = data$weight, area = data$area, domain = data$domain, variable = data$variable) /
    nwos_total(weight = data$weight, area = data$area, domain = data$domain, variable = 1)
}
