#' NWOS Trim Weights
#'
#' This function calculates trimmed weights for the NWOS.
#' @param weight vector of weights per ownership.
#' @param method method to be used for trimming. The only method currently implemented is "iqr_1.5".
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' nwos_weights_trimmed(weights = wi$WEIGHT)

nwos_weights_trimmed <- function(weights, method="iqr_1.5")
{
  w <- weights
  w.tot <- sum(w, na.rm=T)
  w.quant <- quantile(w[w > 0], na.rm=T)
  w.iqr <- w.quant[4] - w.quant[2]
  w.trim <- ifelse(w>(w.quant[4] + (1.5*w.iqr)), w.quant[4] + (1.5*w.iqr),
                   ifelse(w<(w.quant[2] - (1.5*w.iqr)), w.quant[2] - (1.5*w.iqr), w))
  w.trim * (sum(w, na.rm=T) / sum(w.trim, na.rm=T)) # Adjust so totals are the same
}


