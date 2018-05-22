#' NWOS Total
#'
#' Estimators used to calculate NWOS totals and, optionally, associated variances
#' @usage nwosTotal(weight, area=NA, point.count=NA, response=NA, domain=1, stratum.area=1, units="ownerships", variance=T, R=1000)
#' @param weight vector of weights per ownership.
#' @param area vector of area (e.g., forest acres) per ownership.
#' @param point.count vector of counts of sample points per ownership/record.
#' @param response vector with 1 indicating response and 0 nonresponse.
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise.
#' @param stratum.area area for stratum (e.g., acres of family forestland in a state).
#' @param units units of analysis. Permissable values are "ownerships" or "area". Default is "ownerships".
#' @param variance logic variable indicating if variances should be calculated
#' @param R=1000
#' @keywords nwos
#' @details
#' Variances are calculated using a bootstrapping approach.
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' data <- NWOS_DATA_SAMPLE[NWOS_DATA_SAMPLE$SAMPLE==1,]
#' data$WEIGHT <- nwosWeights(data$POINT_COUNT, data$ACRES_FOREST, 1000)
#' data$DOMAIN <- 1
#' nwosTotal(data$WEIGHT, var=F)
#' nwosTotal(weight=data$WEIGHT, point.count=data$POINT_COUNT, response=data$RESPONSE, area=data$ACRES_FOREST,
#' domain=data$DOMAIN, stratum.area=1000)
#' nwosTotal(data$WEIGHT, data$ACRES_FOREST, variance=F, units="area")
#' nwosTotal(weight=data$WEIGHT, point.count=data$POINT_COUNT, response=data$RESPONSE, area=data$ACRES_FOREST,
#' domain=data$DOMAIN, stratum.area=1000, units="area")

nwosTotal <- function(weight, area=NA, point.count=NA, response=NA, domain=1, stratum.area=1, units="ownerships",
                      variance=T, R=1000)
{
  if(units=="ownerships")
    x <- sum(weight * domain) # Ownerships estimator
  else
    x <- sum(weight * domain * area) # Area estimator
  if(variance) # If variances are to be calculated
  {
    require(boot)
    nwosBoot <- function(df, indices, area.s=stratum.area) # Function called by boot
    {
      d <- df[indices,] # Create new data frame with indices
      rr <- nwosResponseRate(d$point.count, d$response) # Calculate response rates
      d$weight <- nwosWeights(d$point.count, d$area, area.s, rr) # Calculate weights
      if(units=="ownerships")
        x <- sum(d$weight * d$domain) # Ownerships estimator
      else
        x <- sum(d$weight * d$domain * d$area) # Area estimator
      return(x)
    }
    data <- data.frame(weight, domain, point.count, response, area) # Create data frame
    data <- data[rep(row.names(data), data$point.count), ] # Replicate records by number of points
    data$point.count <- 1 # Reset points counts
    b <- boot::boot(data=data, statistic=nwosBoot, R=R) # Run bootstrap
    # b <- boot::boot(data=data, statistic=nwosBoot, R=R, parallel="multicore", ncpus=3) # Run bootstrap
    x.var=as.numeric(var(b$t)) # Varaince of bootstrap estimates
  }
  else
    x.var <- NA

return(list(x=x,x.var=x.var)) # Return estimate
}
