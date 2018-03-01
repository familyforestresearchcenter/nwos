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
#' 

library(snow)
library(parallel)
detectCores()

# cl <- makeCluster(2, type = "SOCK")
# clusterApply(cl, 1:2, get("+"), 3)
# stopCluster(cl)

nwosVarApply <- function(weight, point.count, domain, y, area, response,
                         stratum.area, units="ownerships", stat="total", R=1000)
{
  df <- data.frame(weight, point.count, domain, y, area, response)
  df <- df[rep(row.names(df), df$point.count), ]
  df$point.count <- 1
  df$response <- ifelse(df$domain==1, as.numeric(as.character(df$response)), NA)
  n <- NROW(df)
  
  # Create Dataframe
  df.rep <- function(x)
  {
    data.frame(rep=x, df[sample(row.names(df), n, replace=T), ])
  }
  
  df.x <- lapply(1:R,df.rep)
  
  # Generate Estimats
  #   if(stat=="total")
  nwosTotal.rep <- function(r)
  {
    df.r <- df.x[[r]]
    rr <- nwosResponseRates(df.r$point.count, df.r$response)
    w <- nwosWeights(df.r$point.count, df.r$area, df.r$domain, stratum.area, rr)
    x <- nwosTotal(weight=w, point.count=df.r$point.count,
                   domain=df.r$domain, y=df.r$y, df.r$area, units=units)
    return(x)
  }
  
  x <- sapply(1:R,nwosTotal.rep)
  
  #   if(stat=="mean")
  #   if(stat=="proportion")
  
  x.var <- var(x)
  
  return(x.var)
}
