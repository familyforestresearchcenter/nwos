#' NWOS Variance
#'
#' This function calculates variances for NWOS statistics using a bootstrapping approach.
#' @param
#' @return
#' @keywords nwos
#' @export
#' @examples
#' nwos.data <- sample.data[sample.data$sample==1,]
#' nwos.data <- sample.data[sample.data$sample==10,]
#' weight=sample.i$weight
#' point.count=sample.i$point.count
#' domain=sample.i$domain
#' y=sample.i$y
#' area=sample.i$area
#' response=sample.i$response
#' stratum.area
#' units="ownerships"
#' stat="total"
#' R=10
#' nwosVarBoot(weight, point.count, domain, y, area, response,stratum.area)


nwosVarBoot <- function(weight, point.count, domain, y, area, response,
                         stratum.area, units="ownerships", stat="total", R=1000)
{
  df <- data.frame(weight, point.count, domain, y, area, response)
  df <- df[rep(row.names(df), df$point.count), ]
  df$point.count <- 1
  df$response <- ifelse(df$domain==1, as.numeric(as.character(df$response)), NA)
  
  # if(stat=="total")
  nwosTotalBoot <- function(df, indices)
  {
    d <- df[indices,]
    rr <- nwosResponseRates(d$point.count, d$response)
    w <- nwosWeights(d$point.count, d$area, d$domain, stratum.area, rr)
    x <- nwosTotal(weight=w, point.count=d$point.count,
                   domain=d$domain, y=d$y, d$area, units=units)
    return(x)
  }
  
  b <- boot(data=df, statistic=nwosTotalBoot,  R=R)

  x.var <- var(b$t)
  
  return(x.var)
}
