#' NWOS Weights
#'
#' This function calculates weights for the NWOS.
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

nwosWeights <- function(nwos.data, stratum.area, response.rate)
{
  #### Data checks ####
  # All stratum are in stratum.area
  # if(!(unique(stratum) %in% stratum.area$stratum))
  #   stop("All strata must be in stratum.area")
  # All stratum are in response.rate
  # if(!(unique(data.frame(stratum,domain)) %in% unique(data.frame(response.rate$stratum,response.rate$domain))))
  #   stop("All strata/domains must be in repose.rate")

  #### Calculate Weights ####
  weight <- data.frame(id=character(0),
                       weight=numeric(0))
  stratum.list <- unique(nwos.data$stratum)
  for(s in stratum.list) # By stratum
  # s <- "WI"
  {
    # Sample size
    n <- sum(nwos.data$point.count[nwos.data$stratum==s])

    # Calcuate weights for private ownerships
    domain.private.forest <- c("FamilyForest", "CorporateForest", "OtherPrivateForest")
    # d <- "FamilyForest"
    for(d in domain.private.forest)
    {
      if(sum(nwos.data$point.count[nwos.data$stratum==s & nwos.data$domain==d])>0)
      {
        # Calculate base weights
        w.s.d <- stratum.area$area[stratum.area$stratum==s] /
          (n * nwos.data$area[nwos.data$stratum==s & nwos.data$domain==d & nwos.data$response==1])

        # Adjust for forest points with unkown ownership
        # w.s.d <- w.s.d * (1/(1-prop.unknown.forest))

        # Adjust for domain response rates
        rr.s.d <- response.rate$response.rate[response.rate$stratum==s & response.rate$domain==d]
        w.s.d <- w.s.d * (1/rr.s.d)
        # sum(w.s.d)
        w.s.d.id <- nwos.data$id[nwos.data$stratum==s & nwos.data$domain==d & nwos.data$response==1]

        # Append to weight vector
        weight <- data.frame(rbind(weight,
                                   data.frame(id=w.s.d.id, weight=w.s.d)))
      } # End domain if
    } # End domain loop
  } # End stratum loop
  nwos.data$weight <- weight$weight[match(nwos.data$id, weight$id)]
  return(nwos.data)
}
