#' NWOS Variance
#'
#' This function calculates variances for NWOS statistics using a bootstrapping approach.
#' @usage nwosVar(point.count, domain, y=1, area, response=1,
#' stratum.area, units="ownerships", stat="total", R=100)
#' @param data data frame of survey data. See details below for required variables.
#' @param units units of analysis. Permissable values are "ownerships" or "area". Default is "ownerships".
#' @param stat statistic of interest. Permissable values are "total", "mean" or "proportion". Default is "total".
#' @param R number of replicates or bootstraps. Default is 100.
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @keywords nwos
#' @export
#' @examples
#' load("data/nwos_data_sample.RData")
#' df <- nwos.data.sample[nwos.data.sample$SAMPLE==1,]
#' nwosVar(point.count=df$POINT_COUNT, area=df$ACRES_FOREST, area.total=1000, R=100)

nwosVar <- function(data, state.areas, response.rates,
                    units="ownerships", stat="total", R=100)
{
  require(boot)

  out <- data.frame(STATE=numeric(0),
                    STRATUM=numeric(0),
                    VARIANCE=numeric(0)) # Create empty data frame

  state <- unique(data$STATE) # Vector of unique states in data
  for(i in state) # State loop
  {
    data.i <- data[data$STATE%in%i,] # Data for state i
    stratum <- unique(data$STRATUM[data$STATE%in%i])  # Vector of unique strata in state i
    for(j in stratum) # Stratum loop
    {
      data.i.j <- data.i[data.i$STRATUM%in%j, ] # Data for stratum j in state i
      # if(units=="ownerships")
      #   x <- sum(data.i.j$WEIGHT * data.i.j$DOMAIN, na.rm=T) # Ownerships estimator
      # if(units=="area")
      #   x <- sum(data.i.j$WEIGHT * data.i.j$DOMAIN * data.i.j$AREA, na.rm=T) # Area estimator

      # df <- data.frame(point.count, area, stratum, domain, y, response)
      data.i.j.rep <- data.i.j[rep(row.names(data.i.j), data.i.j$POINT_COUNT), ]
      data.i.j.rep$POINT_COUNT <- 1

      if(stat=="total")
        nwosBoot <- function(df, indices)
        {
          d <- df[indices,]
          rr <- nwosResponseRate(d)
          w <- nwosWeights(d$point.count, d$area, area.total, rr, stratum=d$stratum)
          # nwosWeights <- function(data, state.areas, response.rates)
          x <- nwosTotal(data, units=units)
          return(x)
        }

      if(stat=="mean")
        nwosBoot <- function(df, indices)
        {
          d <- df[indices,]
          rr <- nwosResponseRate(d$point.count, d$response, d$stratum)
          w <- nwosWeights(d$point.count, d$area, area.total, rr, stratum=d$stratum)
          x <- nwosMean(weight=w, y=d$y, stratum=d$stratum, domain=d$domain, d$area, units=units)
          return(x)
        }

      if(stat=="proportion")
        nwosBoot <- function(df, indices)
        {
          d <- df[indices,]
          rr <- nwosResponseRate(d$point.count, d$response, d$stratum)
          w <- nwosWeights(d$point.count, d$area, area.total, rr, stratum=d$stratum)
          x <- nwosProportion(weight=w, y=d$y, stratum=d$stratum, domain=d$domain, d$area, units=units)
          return(x)
        }


      b <- boot::boot(data=data.i.j.rep, statistic=nwosBoot,  R=R)

      out <- rbind(out,
                   data.frame(STATE=i,
                              STRATUM=j,
                              VARIANCE=as.numeric(var(b$t)))) # Append estimate to data frame
    } # End stratum loop
  } # End state loop







  # df <- data.frame(point.count, area, stratum, domain, y, response)
  # df <- df[rep(row.names(df), df$point.count), ]
  # df$point.count <- 1
  #
  # if(stat=="total")
  #   nwosBoot <- function(df, indices)
  #   {
  #     d <- df[indices,]
  #     rr <- nwosResponseRate(d$point.count, d$response, d$stratum)
  #     w <- nwosWeights(d$point.count, d$area, area.total, rr, stratum=d$stratum)
  #     x <- nwosTotal(weight=w, stratum=d$stratum, domain=d$domain, y=d$y, d$area, units=units)
  #     return(x)
  #   }
  #
  # if(stat=="mean")
  #   nwosBoot <- function(df, indices)
  #   {
  #     d <- df[indices,]
  #     rr <- nwosResponseRate(d$point.count, d$response, d$stratum)
  #     w <- nwosWeights(d$point.count, d$area, area.total, rr, stratum=d$stratum)
  #     x <- nwosMean(weight=w, y=d$y, stratum=d$stratum, domain=d$domain, d$area, units=units)
  #     return(x)
  #   }
  #
  # if(stat=="proportion")
  #   nwosBoot <- function(df, indices)
  #   {
  #     d <- df[indices,]
  #     rr <- nwosResponseRate(d$point.count, d$response, d$stratum)
  #     w <- nwosWeights(d$point.count, d$area, area.total, rr, stratum=d$stratum)
  #     x <- nwosProportion(weight=w, y=d$y, stratum=d$stratum, domain=d$domain, d$area, units=units)
  #     return(x)
  #   }
  #
  # b <- boot::boot(data=df, statistic=nwosBoot,  R=R)
  #
  # x.var <- as.numeric(var(b$t))

  return(out)
}
