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

nwosQuantile <- function(nwos.data, 
                         stratum="WI", 
                         domain="FamilyForest", 
                         subdomain=rep(1, NROW(nwos.data)), 
                         subdomain.name=NA,
                         subsubdomain=rep(1, NROW(nwos.data)), 
                         subsubdomain.name=NA,
                         units=c("ownerships","area"), 
                         prob=c(0.00, 0.25, 0.50, 0.75, 1.00), 
                         max.iter=1000)
{
  out.df <- data.frame(stratum=character(0),
                       domain=character(0),
                       subdomain=character(0),
                       subsubdomain=character(0),
                       value=numeric(0),
                       units=character(0),
                       stat=character(0))
  for(st in 1:NROW(stratum))# By stratum
  {
    for(d in 1:NROW(domain)) # By domain
    {
      for(u in 1:NROW(units)) # By units
      {
        total <- nwosTotal(nwos.data, 
                           stratum=stratum[st], 
                           domain=domain[d], 
                           subdomain=subdomain, 
                           subdomain.name=subdomain.name,
                           subsubdomain=subsubdomain, 
                           subsubdomain.name=subsubdomain.name,
                           units=units[u])$value
        x <- nwos.data$area[nwos.data$domain==domain[d] & nwos.data$response==1]
        x.min <- min(x, na.rm=T)
        x.max <- max(x, na.rm=T)
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
                  total.quant.iter <- nwosTotal(nwos.data, stratum=stratum[st], domain=domain[d], 
                                                subdomain=subdomain, 
                                                subdomain.name=NA,
                                                subsubdomain=subsubdomain * ifelse(nwos.data$area>=x.quant.iter, 1, 0), 
                                                subsubdomain.name=NA,
                                                units=units[u])$value
                  quant.iter <- total.quant.iter / total
                  quant.iter.diff <- ifelse((x.quant.prev - x.quant.iter) > 10, 
                                            (x.quant.prev - x.quant.iter) / 2,
                                            x.quant.iter * 0.01)
                  x.quant.prev <- x.quant.iter
                  if(quant.iter<(1-prob[p])) x.quant.iter <- x.quant.iter - quant.iter.diff 
                  if(quant.iter>=(1-prob[p])) x.quant.iter <- x.quant.iter + quant.iter.diff
                  # print(paste(it, quant.iter, x.quant.prev))
                }
              }
            } # End else loop
            out.df <- data.frame(rbind(out.df,
                                       data.frame(stratum=stratum[st],
                                                  domain=domain[d],
                                                  subdomain=subdomain.name,
                                                  subsubdomain=subsubdomain.name,
                                                  value=x.quant.prev,
                                                  units=units[u],
                                                  stat=paste("quant.prob.", prob[p], sep=""))))
        } # End prob loop
      } # End units loop
    } # End domain loop
  } # End stratum loop
  # print(out.df)
  return(out.df)
}
