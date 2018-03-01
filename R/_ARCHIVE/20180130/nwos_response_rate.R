#' NWOS Response Rates
#'
#' This function calculates response rates for the NWOS by stratum and domain.
#' @param point.count
#' @param response Indicator variable for whether an ownership responded. 1=Yes and 0=No.
#' @details
#' @return
#' @keywords nwos
#' @export
#' @examples

nwosResponseRates <- function(point.count, response)
{
    rr <- sum(point.count[response%in%c(1)]) / sum(point.count[response%in%c(1,0)])
    return(rr)
}
