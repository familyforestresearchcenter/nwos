#' NWOS Total
#'
#' This function calculates totals for the NWOS.
#' @param weight
#' @param point.count
#' @param domain
#' @param y
#' @param area
#' @param units =c("ownerships","area"). Default is "ownerships".
#' @return
#' @keywords nwos
#' @export
#' @examples
#' load("ANALYSIS/R/nwos/data/nwos_data_20180107.RData")
#' nwos.response.rate <- nwosResponseRates(nwos.data)
#' nwos.data.weights <-
#'     nwosWeights(nwos.data,
#'     stratum.area=data.frame(stratum="WI", area=35198019),
#'     response.rate=nwos.response.rate)
#' nwos.data <- subset(nwos.data.sample, sample==1)
#' nwosTotal(nwos.data.weights, subdomain=ifelse(nwos.data.weights$area>=10, 1, 0), subdomain.name="tenPlus")
#' nwosTotal(subset(nwos.data.sample, sample==1))

nwosTotal <- function(weight, point.count, domain, y, area, units="ownerships")
{
  if(units=="ownerships")
    x <- sum(weight * point.count * domain * y, na.rm=T)
  else
    x <- sum(weight * point.count * domain * y * area, na.rm=T)
  return(x)
}
