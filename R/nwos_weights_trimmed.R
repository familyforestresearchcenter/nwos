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
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0), RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0), RESPONSE = if_else(is.na(RESPONSE_PROPENSITY), 0, RESPONSE))
#' WI_FFO_AREA <- nwos_stratum_area(stratum = wi$FFO, point.count = wi$POINT_COUNT, state.area = 33898733)
#' WI_FFO_RR <- nwos_response_rate(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE)
#' wi$WEIGHT <- nwos_weights(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE, area = wi$AC_WOOD, stratum.area = WI_FFO_AREA, response.rate = WI_FFO_RR)
#' wi$WEIGHT_TRIMMED <- nwos_weights_trimmed(weights = wi$WEIGHT)
#' summary(wi$WEIGHT[wi$WEIGHT > 0])
#' summary(wi$WEIGHT_TRIMMED[wi$WEIGHT_TRIMMED > 0])

nwos_weights_trimmed <- function(weights, method = "iqr_1.5")
{
  w <- weights
  w.tot <- sum(w, na.rm = T)
  w.quant <- quantile(w[w > 0], na.rm = T)
  w.iqr <- w.quant[4] - w.quant[2]
  w.trim <- ifelse(w>(w.quant[4] + (1.5*w.iqr)), w.quant[4] + (1.5*w.iqr),
                   ifelse(w<(w.quant[2] - (1.5*w.iqr)), w.quant[2] - (1.5*w.iqr), w))
  w.trim * (sum(w, na.rm=T) / sum(w.trim, na.rm=T)) # Adjust so totals are the same
}
