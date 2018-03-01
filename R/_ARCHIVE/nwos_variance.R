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

# weight=sample.i$weight
# point.count=sample.i$point.count
# domain=sample.i$domain
# y=sample.i$y
# area=sample.i$area
# response=sample.i$response
# stratum.area
# units="ownerships"
# stat="total"
# R=10


nwosVar <- function(weight, point.count, domain, y, area, response,
                    stratum.area, units="ownerships", stat="total", R=100)
{
  df <- data.frame(weight, point.count, domain, y, area, response)
  df$response <- ifelse(df$domain==1, as.numeric(as.character(df$response)), NA)

  x <- numeric(0)

  for(r in 1:R) # By number of replicates
  {
    # Resample
    # df.r <- df[sample(row.names(df), NROW(df), replace=T, prob=df$point.count), ]
    df.r <- df[sample(row.names(df), sum(df$point.count), replace=T, prob=df$point.count), ]
    df.r$point.count <- 1

    # Response Rates
    rr <- nwosResponseRates(df.r$point.count, df.r$response)

    # Weights
    w <- nwosWeights(df.r$point.count, df.r$area, df.r$domain, stratum.area, rr)

    # Estimate
    if(stat=="total")
      x.r <- nwosTotal(weight=w, point.count=df.r$point.count,
                     domain=df.r$domain, y=df.r$y, df.r$area, units=units)
    if(stat=="mean")
      x.r <- nwosMean(weight=w, point.count=df.r$point.count,
                       domain=df.r$domain, y=df.r$y, df.r$area, units=units)
    if(stat=="proportion")
      x.r <- nwosProportion(weight=w, point.count=df.r$point.count,
                      domain=df.r$domain, y=df.r$y, df.r$area, units=units)
    # print(x.r)
    x <- c(x,x.r)
  } # End replicate loop

  x.var <- var(x)

  return(x.var)
}
