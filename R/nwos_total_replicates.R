#' NWOS Replicate Totals
#'
#' Estimators used to calculate NWOS totals for replicates. This is typically used with an apply function.
#' @usage nwos_total_replicates <- function(r, index.rep, data, weight.rep, owner.area.name = "OWNER", domain.name = "FFO", variable.name = "FFO")
#' @param r vector of replicates. See details below.
#' @param index.rep list of observations (i.e., replicates) in data to include.
#' @param data data frame containing stratum variable.
#' @param weight.rep list with each element the weights for the observations in that replicate.
#' @param owner.area.name name of a variable in data of the area (of forestland) owned by each ownership. Default = "OWNER" (i.e., 1, which is used for estimating number of ownerships).
#' @param domain.name name of the variable in data indicating inclusion (1) and exclusion (0) in the domain of interest. Default = "FFO".
#' @param variable.name name of the variable of interest in data. Default = "FFO". See details below.
#' @keywords nwos
#' @details
#' r it typcially set using 1:length(WI_REPLICATES)
#' For estiamtes in terms ownerships, a dummy variable (e.g., OWNER=1) needs to be added.
#' Variable can be binary or continuous. If variable is binary, the value returned is the number of ownerships or acreage in the domain of interest with that attribute. If variable is continuous, the value returned is the variable total in terms of ownerships or acreage.
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- wi %>% mutate(OWNER = 1)
#' WI_FFO_OWN_TOTAL_REP <- sapply(1:length(WI_REPLICATES), nwos_total_replicates, index.rep = WI_REPLICATES, data = wi,
#' weight.rep = WI_FFO_WEIGHTS_REP, owner.area.name = "OWNER",
#' domain.name = "FFO", variable.name = "FFO")
#' var(WI_FFO_OWN_TOTAL_REP)

nwos_total_replicates <- function(r, index.rep, data,
                                  weight.rep,
                                  owner.area.name = "OWNER",
                                  domain.name = "FFO",
                                  variable.name = "FFO") {
  data <- data.frame(weight = weight.rep[[r]],
                     area = unlist(data[index.rep[[r]], owner.area.name]),
                     domain = unlist(data[index.rep[[r]], domain.name]),
                     variable = unlist(data[index.rep[[r]], variable.name])) # Create data frame
  nwos_total(weight = data$weight, area = data$area, domain = data$domain, variable = data$variable)
}
