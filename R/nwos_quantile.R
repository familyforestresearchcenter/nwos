#' NWOS Quantile
#'
#' This function calculates quantiles for NWOS estimates.
#' @usage nwosQuantile(weight, point.count, domain, y=1, area, units="ownerships",
#' prob=c(0.00, 0.25, 0.50, 0.75, 1.00), max.iter=1000)
#' @param weight weight for each observation.
#' @param point.count vector of number of sample points. Needs to sum to total number of sample points across all land uses and ownership classes.
#' @param domain variable indicating whether ownership is in the domain of interest.
#' @param y variable of interest. Set to 1 if interested in basic ownership or area totals. Default is 1.
#' @param area vector of areas of forest land owned by sampled ownerships. NAs are permissable.
#' @param units units of analysis. Permissable values are "ownerships" or "area". Default is "ownerships".
#' @param prob vector of probabilities for quantiles.
#' @param max.iter maximum number of iterations. Default = 100.
#' @keywords nwos
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_sample_data.RData")
#' nwos.sample.data$domain <- ifelse(nwos.sample.data$owner.class=="FamilyForest", 1, 0)
#' nwos.sample.data$response <- ifelse(nwos.sample.data$owner.class=="FamilyForest",
#' as.numeric(as.character(nwos.sample.data$response)), NA)
#' nwos.sample.data$weights <- nwosWeights(point.count=nwos.sample.data$point.count,
#' area=nwos.sample.data$area,
#' domain=nwos.sample.data$domain,
#' stratum.area=35198019,
#' response.rate=sample.response.rate)
#' nwosQuantile(weight=nwos.sample.data$weight,
#' point.count=nwos.sample.data$point.count,
#' domain=nwos.sample.data$domain,
#' area=nwos.sample.data$area)

nwosQuantile <- function(weight, point.count, domain, y=1, area, units="ownerships",
                         prob=c(0.00, 0.25, 0.50, 0.75, 1.00), max.iter=1000)
{
  x.quant <- numeric(0)
  total <- nwosTotal(weight, point.count, domain, y, area, units=units)
  x.min <- min(area[domain==1], na.rm=T)
  x.max <- max(area[domain==1], na.rm=T)
  for(p in 1:NROW(prob)) # By probability level
  {
    if(prob[p]==0) x.quant.prev <- x.min
    else
      if(prob[p]==1) x.quant.prev <- x.max
      else
      {
        x.quant.iter <- sum(x.min,x.max) / 2
        x.quant.prev <- x.max
        quant.iter <- 0
        for(it in 1:max.iter)
        {
          if(round(quant.iter,2)!=(1-prob[p]))
          {
            total.quant.iter <- nwosTotal(weight, point.count, domain,
                                          y=y*ifelse(area>=x.quant.iter, 1, 0),
                                          area, units=units)
            # total.quant.iter <- nwosTotal(nwos.data, stratum=stratum[st], domain=domain[d],
            #                               subdomain=subdomain,
            #                               subdomain.name=NA,
            #                               subsubdomain=subsubdomain * ifelse(nwos.data$area>=x.quant.iter, 1, 0),
            #                               subsubdomain.name=NA,
            #                               units=units[u])$value
            quant.iter <- total.quant.iter / total
            quant.iter.diff <- ifelse((x.quant.prev - x.quant.iter) > 10,
                                      (x.quant.prev - x.quant.iter) / 2,
                                      x.quant.iter * 0.01)
            x.quant.prev <- x.quant.iter
            if(quant.iter<(1-prob[p])) x.quant.iter <- x.quant.iter - quant.iter.diff
            if(quant.iter>=(1-prob[p])) x.quant.iter <- x.quant.iter + quant.iter.diff
          }
        }
      } # End else loop
      x.quant <- c(x.quant, x.quant.prev)
  } # End prob loop
  names(x.quant) <- prob
  return(x.quant)
}
