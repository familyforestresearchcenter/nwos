#' NWOS Trim Weights
#'
#' This function calculates trimmed weights for the NWOS.
#' @param weights weight for each observation.
#' @param method method to be used for trimming. The only current method implemented is "iqr_1.5".
#' @keywords nwos
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' # Calculate trimmed weights for family forest ownerships
#' load("data/nwos_sample_data.RData")
#' nwos.sample.data$domain <- ifelse(nwos.sample.data$owner.class=="FamilyForest", 1, 0)
#' nwos.sample.data$response <- ifelse(nwos.sample.data$owner.class=="FamilyForest",
#' as.numeric(as.character(nwos.sample.data$response)), NA)
#' sample.response.rate <- nwosResponseRates(nwos.sample.data$point.count,nwos.sample.data$response)
#' nwos.sample.data$weights <- nwosWeights(point.count=nwos.sample.data$point.count,
#' area=nwos.sample.data$area,
#' domain=nwos.sample.data$domain,
#' stratum.area=35198019,
#' response.rate=sample.response.rate)
#' nwos.sample.data$weights.trimmed <- nwosTrimWeights(nwos.sample.data$weights)

nwosTrimWeights <- function(weights, method="iqr_1.5")
{
  w <- weights
  w.tot <- sum(w, na.rm=T)
  w.quant <- quantile(w, na.rm=T)
  w.iqr <- w.quant[4] - w.quant[2]
  names(w.iqr) <- "iqr"
  w.trim <- ifelse(w>(w.quant[4] + (1.5*w.iqr)), w.quant[4] + (1.5*w.iqr),
                   ifelse(w<(w.quant[2] - (1.5*w.iqr)), w.quant[2] - (1.5*w.iqr), w))
  w.trim <- w.trim * (sum(w, na.rm=T) / sum(w.trim, na.rm=T))
  return(w.trim)
}


