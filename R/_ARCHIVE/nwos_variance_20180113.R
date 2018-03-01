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


nwosVar <- function(nwos.data,
                     stratum="WI",
                     domain="FamilyForest",
                     subdomain=rep(1, NROW(nwos.data)),
                     subdomain.name=NA,
                     subsubdomain=rep(1, NROW(nwos.data)),
                     subsubdomain.name=NA,
                     units=c("ownerships","area"),
                     stat="total",
                     R=100)
{
  nwos.data$subdomain <- subdomain
  nwos.data$subsubdomain <- subsubdomain
  # nwos.data$response[is.na(nwos.data$response)] <- 0
  out.df <- data.frame(stratum=character(0),
                       domain=character(0),
                       subdomain=character(0),
                       subsubdomain=character(0),
                       value=numeric(0),
                       units=character(0),
                       stat=character(0),
                       boot=character(0))
  for(st in 1:NROW(stratum)) # By stratum
  {
    for(dom in 1:NROW(domain))
    {
      # Responses
      sub <- nwos.data$domain==domain[dom] & nwos.data$response==1
      responses <- subset(nwos.data, sub)
      non.responses <- subset(nwos.data, !sub)

      for(r in 1:R) # By number of replicates
      {
        # print(r)
        # Resample
        sam <- sample(1:NROW(responses$id), NROW(responses), replace=T, prob=responses$point.count)

        # Rebuild
        df <- data.frame(rbind(responses[sam,], non.responses))

        # Response Rates
        rr <- nwosResponseRates(df)

        # Weights
        w <- nwosWeights(df, stratum.area=stratum.area, response.rate=rr)

        # Estimate
        if(stat=="total") {
          boot.df <- nwosTotal(w,
                               stratum=stratum,
                               domain=domain,
                               subdomain=df$subdomain,
                               subdomain.name=subdomain.name,
                               subsubdomain=df$subsubdomain,
                               subsubdomain.name=subsubdomain.name,
                               units=units)
        }
        if(stat=="mean")
        {
          boot.df <- nwosMean(w,
                              stratum=stratum,
                              domain=domain,
                              subdomain=df$subdomain,
                              subdomain.name=subdomain.name,
                              subsubdomain=df$subsubdomain,
                              subsubdomain.name=subsubdomain.name,
                              units=units)
        }

        out.df <- data.frame(rbind(out.df,
                                   data.frame(boot.df,
                                              boot=rep(r,NROW(total.df)))))
        # x <- c(x,x.R)
      } # End replicate loop
    } # End domain loop
  } # End stratim loop
  return(out.df)
}

# tapply(out.df$value, out.df$units, summary)
