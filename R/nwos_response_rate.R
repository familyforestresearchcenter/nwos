#' NWOS Response Rate
#'
#' This function calculates response rate for the NWOS.
#' @usage nwosResponseRate(point.count, response)
#' @param point.count vector of counts of sample points per ownership/record.
#' @param response vector with 1 indicating response and 0 nonresponse.
#' @details
#' @return response rate
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' data <- NWOS_DATA_SAMPLE[NWOS_DATA_SAMPLE$SAMPLE==1,]
#' nwosResponseRate(data$POINT_COUNT, data$RESPONSE)

nwosResponseRate <- function(point.count, response)
{
  rr <- sum(point.count[response%in%c(1)]) / sum(point.count[response%in%c(1,0)]) # Calcuate respons rate
  return(rr) # Return response rate
}
