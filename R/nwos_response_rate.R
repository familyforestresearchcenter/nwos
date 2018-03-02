#' NWOS Response Rates
#'
#' This function calculates response rates for the NWOS by stratum and domain.
#' @usage nwosResponseRates(point.count, response)
#' @param point.count vector of number of sample points. Needs to sum to total number of sample points across all land uses and ownership classes.
#' @param response Indicator variable for whether an ownership responded. 1=Yes and 0=No.
#' @return response rate
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' ## Calculate response rate for family forest ownerships
#' load("data/nwos_sample_data.RData")
#' nwos.sample.data$response <- ifelse(nwos.sample.data$owner.class=="FamilyForest",
#' as.numeric(as.character(nwos.sample.data$response)), NA)
#' nwosResponseRates(nwos.sample.data$point.count,nwos.sample.data$response)

nwosResponseRates <- function(point.count, response)
{
    rr <- sum(point.count[response%in%c(1)]) / sum(point.count[response%in%c(1,0)])
    return(rr)
}
