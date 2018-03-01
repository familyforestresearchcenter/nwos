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
#' nwosWeights(nwos.data=nwos.df,
#'     stratum.area=data.frame(stratum="WI", area=41918720),
#'     response.rate=nwos.rr)
#'
#'     id=nwos.data$id,
#'     area=nwos.data$area,
#'     point.count=nwos.data$point.count,
#'     response=nwos.data$response,
#'     stratum=nwos.data$stratum,
#'     domain=nwos.data$domain,

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
    n

    # Calculate adjustment factor to account for forested points with unknown ownership codes
    domain.forest <- c("FamilyForest", "CorporateForest", "OtherPrivateForest", "TribalForest",
                       "FederalForest", "StateForest", "LocalForest", "UnknownForest")
    if(sum(point.count[domain=="UnknownForest"])==0)
      prop.unknown.forest <- 1
    else
      prop.unknown.forest <- sum(point.count[domain=="UnknownForest"]) / sum(point.count[domain%in%domain.forest])
    prop.unknown.forest

    # Calcuate weights for private ownerships
    domain.private.forest <- c("FamilyForest", "CorporateForest", "OtherPrivateForest")
    # d <- "FamilyForest"
    for(d in domain.private.forest)
    {
      if(sum(point.count[stratum==s & domain==d])>0)
      {
        w.s.d <- stratum.area$area[stratum.area$stratum==s] / (n * area[stratum==s & domain==d & response==1])
        summary(w.s.d)
        sum(w.s.d)
        # sum(w.s.d * (1/rr.s.d))
        # Adjust for forest points with unkown ownership
        w.s.d <- w.s.d * (1/prop.unknown.forest)

        # Adjust for domain response rates
        rr.s.d <- response.rate$response.rate[response.rate$stratum==s & response.rate$domain==d]
        w.s.d <- w.s.d * (1/rr.s.d)
        w.s.d.id <- id[stratum==s & domain==d & response==1]
        sum(w.s.d)

        # Append to weight vector
        weight <- data.frame(rbind(weight,
                                   data.frame(id=w.s.d.id, weight=w.s.d)))
      } # End domain if
    } # End domain loop
  } # End stratum loop
  return(weight)
}
