#' NWOS Variance
#'
#' This function calculates variances for NWOS statistics using a bootstrapping approach.
#' @usage nwosVar(weight, point.count, domain, y=1, area, response, stratum.area, units="ownerships", stat="total", R=2500)
#' @param weight weight for each observation.
#' @param point.count vector of number of sample points. Needs to sum to total number of sample points across all land uses and ownership classes.
#' @param domain variable indicating whether ownership is in the domain of interest.
#' @param y variable of interest. Set to 1 if interested in basic ownership or area totals. Default is 1.
#' @param area vector of areas of forest land owned by sampled ownerships. NAs are permissable.
#' @param response Indicator variable for whether an ownership responded. 1=Yes and 0=No.
#' @param units units of analysis. Permissable values are "ownerships" or "area". Default is "ownerships".
#' @param stat statistic of interest. Permissable values are "total", "mean" or "proportion". Default is "total".
#' @param R number of replicates or bootstraps. Default is 2500.
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @keywords nwos
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
#' # Variance of total number of ownerships
#' nwosVar(weight=nwos.sample.data$weight,
#' point.count=nwos.sample.data$point.count,
#' domain=nwos.sample.data$domain,
#' area=nwos.sample.data$area,
#' response=nwos.sample.data$response,
#' stratum.area=35198019,
#' units="ownerships",
#' stat="total")
#' #' # Variance of total number of acres
#' nwosVar(weight=nwos.sample.data$weight,
#' point.count=nwos.sample.data$point.count,
#' domain=nwos.sample.data$domain,
#' area=nwos.sample.data$area,
#' response=nwos.sample.data$response,
#' stratum.area=35198019,
#' units="area",
#' stat="total")

nwosVar <- function(weight, point.count, domain, y=1, area, response,
                         stratum.area, units="ownerships", stat="total", R=2500)
{
  library(boot)
  df <- data.frame(weight, point.count, domain, y, area, response)
  df <- df[rep(row.names(df), df$point.count), ]
  df$point.count <- 1
  df$response <- ifelse(df$domain==1, as.numeric(as.character(df$response)), NA)

  if(stat=="total")
  nwosBoot <- function(df, indices)
  {
    d <- df[indices,]
    rr <- nwosResponseRates(d$point.count, d$response)
    w <- nwosWeights(d$point.count, d$area, d$domain, stratum.area, rr)
    x <- nwosTotal(weight=w, point.count=d$point.count,
                   domain=d$domain, y=d$y, d$area, units=units)
    return(x)
  }

  if(stat=="mean")
    nwosBoot <- function(df, indices)
    {
      d <- df[indices,]
      rr <- nwosResponseRates(d$point.count, d$response)
      w <- nwosWeights(d$point.count, d$area, d$domain, stratum.area, rr)
      x <- nwosMean(weight=w, point.count=d$point.count,
                     domain=d$domain, y=d$y, d$area, units=units)
      return(x)
    }

  if(stat=="proportion")
    nwosBoot <- function(df, indices)
    {
      d <- df[indices,]
      rr <- nwosResponseRates(d$point.count, d$response)
      w <- nwosWeights(d$point.count, d$area, d$domain, stratum.area, rr)
      x <- nwosProportion(weight=w, point.count=d$point.count,
                     domain=d$domain, y=d$y, d$area, units=units)
      return(x)
    }

  b <- boot(data=df, statistic=nwosBoot,  R=R)

  x.var <- var(b$t)

  return(x.var)
}
