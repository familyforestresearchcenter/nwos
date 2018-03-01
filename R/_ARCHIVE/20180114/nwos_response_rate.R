#' NWOS Response Rates
#'
#' This function calculates response rates for the NWOS by stratum and domain.
#' @param nwos.data Indicator variable for whether an ownership responded. 1=Yes and 0=No.
#' @details
#' nwos.data needs to have the following variables:
#'     response
#'     stratum Stratum assignment for each ownership in response.
#'     domain Domain assignment for each ownership in response.
#' @return data frame of response rates by stratum and domain.
#' @keywords nwos
#' @export
#' @examples
#' load("ANALYSIS/R/nwos/data/nwos_data_20180107.RData")
#' nwos.response.rate <- nwosResponseRates(nwos.data)
#' nwos.response.rate

nwosResponseRates <- function(nwos.data)
{
  #### Check that all variables are in data frame ####
  if(!(all("response" %in% names(nwos.data), "stratum" %in% names(nwos.data), "domain" %in% names(nwos.data))))
    stop("response, domain, or stratum variable missing from nwos.data")

  #### Set up data ####
  # df <- data.frame(nwos.data$response, nwos.data$stratum, nwos.data$domain)
  rr <- data.frame(stratum=numeric(0), domain=numeric(0), respons.rate=numeric(0))

  #### Calculate response rates ####
  stratum.domain.list <- unique(data.frame(stratum=nwos.data$stratum,domain=nwos.data$domain))
  for(s.d in 1:NROW(stratum.domain.list)) # By stratum
  # s.d <- 6
  {
    df.s.d <- subset(nwos.data, nwos.data$stratum==stratum.domain.list[s.d,1] & nwos.data$domain==stratum.domain.list[s.d,2]) # Data frame for a stratum/domain
    # rr.s.d <- NROW(df.s.d$response[df.s.d$response==1]) / NROW(df.s.d$response) # Response rate for a stratum/domain
    rr.s.d <- sum(df.s.d$point.count[df.s.d$response==1],na.rm=T) / sum(df.s.d$point.count,na.rm=T) # Response rate for a stratum/domain
    rr <- data.frame(rbind(rr,
                           data.frame(stratum=stratum.domain.list[s.d,1],
                                      domain=stratum.domain.list[s.d,2],
                                      response.rate=rr.s.d)))
  }
  rr <- rr[order(rr$stratum,rr$domain),]
  rr$response.rate[!(rr$domain%in%c("FamilyForest","CorporateForest","OtherPrivateForest"))] <- NA
  return(rr)
}
