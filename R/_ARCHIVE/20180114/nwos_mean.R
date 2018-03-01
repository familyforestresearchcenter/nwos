#' NWOS Mean
#'
#' This function calculates totals for the NWOS.
#' @param id Area of land owned by ownership i in stratum s.
#' @param area Area of land owned by ownership i in stratum s.
#' @param point.count Number of sample points in stratum s for owner i.
#' @param response Number of sample points in stratum s for owner i.
#' @param stratum Stratum assignment for each observation in a.
#' @param domain Stratum assignment for each observation in a.
#' @param stratum.area Data frame or matrix with two columns: stratum and area. Area of land for each stratums. Needs to be in the same units as a.
#' @param reponse.rate Data frame or matrix with three columns: stratum, domain, and response.rate. Response rates, as a porportion, for each domain of interest.
#' @return vector of weights with id used as the row names.
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
#' nwosTotal(nwos.data.weights)

nwosMean <- function(nwos.data, 
                     stratum="WI", 
                     domain="FamilyForest", 
                     subdomain=rep(1, NROW(nwos.data)), 
                     subdomain.name=NA,
                     # subsubdomain=rep(1, NROW(nwos.data)),
                     # subsubdomain.name=NA,
                     subsubdomain=nwos.data$area,
                     subsubdomain.name="area",
                     units=c("ownerships","area"))
{
  out.df <- data.frame(stratum=character(0),
                       domain=character(0),
                       subdomain=character(0),
                       subsubdomain=character(0),
                       value=numeric(0),
                       units=character(0),
                       stat=character(0))
  
  subdomain <- ifelse(is.na(subdomain), rep(1, NROW(nwos.data)), subdomain)
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
                                              value=sum(nwos.data.sub$weight*nwos.data.sub$point.count*nwos.data.sub$y, na.rm=T) /
                                                sum(nwos.data.sub$weight, na.rm=T),
                                              units="ownerships",
                                              stat="mean")))
      if("area" %in% units)
        out.df <- data.frame(rbind(out.df,
                                   data.frame(stratum=stratum[st],
                                              domain=domain[dom],
                                              subdomain=subdomain.name,
                                              subsubdomain=subsubdomain.name,
                                              value=sum(nwos.data.sub$weight*nwos.data.sub$point.count*nwos.data.sub$y*nwos.data.sub$area, na.rm=T) /
                                                sum(nwos.data.sub$weight*nwos.data.sub$area, na.rm=T),
                                              units="area",
                                              stat="mean")))
    }
  }
  return(out.df)
}
