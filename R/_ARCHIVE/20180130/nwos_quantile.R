#' NWOS Quantile
#'
#' This function calculates quantiles for NWOS estimates.
#' @param prob vector of probabilities for quantiles.
#' @param max.iter maximum number of iterations. Default = 1,000.
#' @return data frame
#' @keywords nwos
#' @export
#' @examples
#' nwos.data <- nwos.data.sample[nwos.data.sample$sample==1,]

# nwosTotal(weight, point.count, domain, y, area, units="ownerships")

nwosQuantile <- function(weight, point.count, domain, y, area, units="ownerships",
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
