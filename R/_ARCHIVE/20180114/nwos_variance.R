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
  if(any(nwos.data$point.count>1))
  {
    nwos.data <- nwos.data[rep(row.names(nwos.data), nwos.data$point.count), ]
    nwos.data$point.count <- 1
  }
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
    nwos.data.st <- subset(nwos.data, stratum==stratum[st])
    row.names(nwos.data.st) <- 1:NROW(nwos.data.st)
    for(r in 1:R) # By number of replicates
    {
      # print(r)
      # Resample
      nwos.data.st.r <- nwos.data.st[sample(row.names(nwos.data.st), NROW(nwos.data.st), replace=T), ]

      for(dom in 1:NROW(domain))
      {
        # Response Rates
        rr <- nwosResponseRates(nwos.data.st.r)

        # Weights
        w <- nwosWeights(nwos.data.st.r, stratum.area=stratum.area, response.rate=rr)

        # Estimate
        if(stat=="total") {
          boot.df <- nwosTotal(w,
                               stratum=stratum,
                               domain=domain,
                               subdomain=nwos.data.st.r$subdomain,
                               subdomain.name=subdomain.name,
                               subsubdomain=nwos.data.st.r$subsubdomain,
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
