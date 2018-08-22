#' NWOS Mean
#'
#' This function calculates totals for the NWOS.
#' @usage nwosMean(weight, point.count, domain, y=1, area, units="ownerships")
#' @param weight weight for each observation.
#' @param y variable of interest. Set to 1 if interested in basic ownership or area totals. Default is 1.
#' @param domain variable indicating whether ownership is in the domain of interest.
#' @param area vector of areas of forest land owned by sampled ownerships. NAs are permissable.
#' @param units units of analysis. Permissable values are "ownerships" or "area". Default is "ownerships".
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' df <- NWOS_DATA_SAMPLE[NWOS_DATA_SAMPLE$SAMPLE==1,]
#' df$WEIGHT <- nwosWeights(df$POINT_COUNT, df$ACRES_FOREST, 1000)
#' # Mean acreage by ownership
#' nwosMean(weight=df$WEIGHT, y=df$ACRES_FOREST, var=F)
#' # Mean acreage by area
#' nwosMean(weight=df$WEIGHT, y=df$ACRES_FOREST, area=df$ACRES_FOREST, units="area", var=F)

nwosMean <- function(weight, y=1, area=NA, point.count=NA, response=NA, domain=1, stratum.area=1, units="ownerships",
                      variance=T, R=1000)
{
  if(units=="ownerships")
    x <- sum(weight * domain * y) / sum(weight * domain) # Mean ownerships estimator
  else
    x <- sum(weight * domain * area * y) / sum(weight * domain * area) # Mean area estimator
  if(variance) # If variances are to be calculated
  {
    require(boot)
    nwosBoot <- function(df, indices, area.s=stratum.area) # Function called by boot
    {
      d <- df[indices,] # Create new data frame with indices
      rr <- nwosResponseRate(d$point.count, d$response) # Calculate response rates
      d$weight <- nwosWeights(d$point.count, d$area, area.s, rr) # Calculate weights
      if(units=="ownerships")
        x <- sum(d$weight * d$domain * d$y) / sum(d$weight * d$domain) # Ownerships estimator
      else
        x <- sum(d$weight * d$domain * d$area * d$y) / sum(d$weight * d$domain * d$area) # Area estimator
      return(x)
    }
    data <- data.frame(weight, domain, y, point.count, response, area) # Create data frame
    data <- data[rep(row.names(data), data$point.count), ] # Replicate records by number of points
    data$point.count <- 1 # Reset points counts
    b <- boot::boot(data=data, statistic=nwosBoot, R=R) # Run bootstrap
    x.var=as.numeric(var(b$t)) # Varaince of bootstrap estimates
  }
  else
    x.var <- NA

  return(list(x=x,x.var=x.var)) # Return estimate
}
