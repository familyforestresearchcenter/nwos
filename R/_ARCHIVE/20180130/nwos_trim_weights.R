#' NWOS Trim Weights
#'
#' This function calculates totals for the NWOS.
#' @param weights
#' @param method
#' @return
#' @keywords nwos
#' @export
#' @examples
#' load("~/Dropbox (FFRC)/NWOS/GENERAL/PRODUCTS/SCIENCE/WEIGHTING/DATA/SAMPLE/COMBINED/SAMPLE_DATA_W_WEIGHTS.RData")
#' model.data <- subset(sample.data, owner.class=="FamilyForest" & area>=1 & sample==1)
#' nwos.trim.weights(weights=model.data$weight)


nwos.trim.weights <- function(weights, method="iqr_1.5")
{
  w <- weights
  w.tot <- sum(w)
  w.quant <- quantile(w)
  w.iqr <- w.quant[4] - w.quant[2]
  names(w.iqr) <- "iqr"
  w.trim <- ifelse(w>(w.quant[4] + (1.5*w.iqr)), w.quant[4] + (1.5*w.iqr),
                   ifelse(w<(w.quant[2] - (1.5*w.iqr)), w.quant[2] - (1.5*w.iqr), w))
  w.trim <- w.trim * (sum(w) / sum(w.trim))
  return(w.trim)
}


