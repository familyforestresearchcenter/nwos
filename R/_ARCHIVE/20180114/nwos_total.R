#' NWOS Total
#'
#' This function calculates totals for the NWOS.
#' @param
#' @return
#' @keywords nwos
#' @export
#' @examples
#' load("ANALYSIS/R/nwos/data/nwos_data_20180107.RData")
#' nwos.response.rate <- nwosResponseRates(nwos.data)
#' nwos.data.weights <-
#'     nwosWeights(nwos.data,
#'     stratum.area=data.frame(stratum="WI", area=35198019),
#'     response.rate=nwos.response.rate)
#' nwos.data <- subset(nwos.data.sample, sample==1)
#' nwosTotal(nwos.data.weights, subdomain=ifelse(nwos.data.weights$area>=10, 1, 0), subdomain.name="tenPlus")
#' nwosTotal(subset(nwos.data.sample, sample==1))

nwosTotal <- function(nwos.data,
                      stratum="WI",
                      domain="FamilyForest",
                      subdomain=rep(1, NROW(nwos.data)),
                      subdomain.name=NA,
                      subsubdomain=rep(1, NROW(nwos.data)),
                      subsubdomain.name=NA,
                      units=c("ownerships","area"))
{
  out.df <- data.frame(stratum=character(0),
                       domain=character(0),
                       subdomain=character(0),
                       subsubdomain=character(0),
                       value=numeric(0),
                       units=character(0),
                       stat=character(0))

  nwos.data$y <- subdomain * subsubdomain

  for(st in 1:NROW(stratum)) # By stratum
  {
    for(dom in 1:NROW(domain))
    {
      nwos.data.sub <- nwos.data[nwos.data$stratum==stratum[st] & nwos.data$domain==domain[dom],]

      if("ownerships" %in% units)
        out.df <- data.frame(rbind(out.df,
                                   data.frame(stratum=stratum[st],
                                              domain=domain[dom],
                                              subdomain=subdomain.name,
                                              subsubdomain=subsubdomain.name,
                                              value=sum(nwos.data.sub$weight*nwos.data.sub$point.count*nwos.data.sub$y, na.rm=T),
                                              units="ownerships",
                                              stat="total")))
      if("area" %in% units)
        out.df <- data.frame(rbind(out.df,
                                   data.frame(stratum=stratum[st],
                                              domain=domain[dom],
                                              subdomain=subdomain.name,
                                              subsubdomain=subsubdomain.name,
                                              value=sum(nwos.data.sub$weight*nwos.data.sub$point.count*nwos.data.sub$y*nwos.data.sub$area, na.rm=T),
                                              units="area",
                                              stat="total")))
    }
  }
  return(out.df)
}
