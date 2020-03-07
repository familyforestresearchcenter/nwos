#' NWOS Quantile
#'
#' This function calculates quantiles for NWOS estimates.
#' @usage nwos_quantile(weight, area = 1, domain, variable, prob = c(0.00, 0.25, 0.50, 0.75, 1.00), max.iter = 1000)
#'
#' @param weight vector of weights per ownership.
#' @param area vector of area (e.g., forest acres) per ownership. Default = 1 (i.e., estimates are in terms of ownerships).
#' @param domain vector with 1 indicating inclusion in the domain and 0 otherwise. Default = 1 (i.e., all ownerships are included).
#' @param variable vector of variable of interest in terms of the quantile.
#' @param prob vector of probabilities for quantiles. Defualt is 0.00, 0.25, 0.50, 0.75, 1.00 (i.e., quartiles).
#' @param max.iter maximum number of iterations. Default = 1000.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0), RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0), RESPONSE = if_else(is.na(RESPONSE_PROPENSITY), 0, RESPONSE))
#' WI_FFO_AREA <- nwos_stratum_area(stratum = wi$FFO, point.count = wi$POINT_COUNT, state.area = 33898733)
#' WI_FFO_RR <- nwos_response_rate(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE)
#' wi$WEIGHT <- nwos_weights(stratum = wi$FFO, point.count = wi$POINT_COUNT, response = wi$RESPONSE, area = wi$AC_WOOD, stratum.area = WI_FFO_AREA, response.rate = WI_FFO_RR)
#' nwos_quantile(weight = wi$WEIGHT, domain = wi$FFO, variable = wi$AC_WOOD)
#' nwos_quantile(weight = wi$WEIGHT, area = wi$AC_WOOD, domain = wi$FFO, variable = wi$AC_WOOD)

nwos_estimates_quantile <- function(weight, area = 1, stratum = 1, domain = 1, variable,
                          prob = c(0.00, 0.25, 0.50, 0.75, 1.00), max.iter = 1000)
{
  x.quant <- numeric(0)
  total <- nwos_estimates_total(weight = weight, area = area, stratum = stratum, domain = domain)

  x.min <- min(variable[stratum == 1 & domain == 1 & weight > 0], na.rm=T)
  x.max <- max(variable[stratum == 1 & domain == 1 & weight > 0], na.rm=T)

  for(p in 1:NROW(prob)) # By probability level
  {
    if(prob[p]==0) x.quant.prev <- x.min
    else
      if(prob[p]==1) x.quant.prev <- x.max
      else
      {
        x.quant.iter <- sum(x.min, x.max) / 2 # Midpoint
        x.quant.prev <- x.max
        quant.iter <- 0
        for(it in 1:max.iter)
        {
          if(round(quant.iter,2) != (1-prob[p]))
          {
            total.quant.iter <- nwos_estimates_total(weight = weight, area = area, stratum = stratum,
                                                     domain = domain * ifelse(variable >= x.quant.iter, 1, 0))
            quant.iter <- total.quant.iter / total
            quant.iter.diff <- ifelse((x.quant.prev - x.quant.iter) > 10,
                                      (x.quant.prev - x.quant.iter) / 2,
                                      x.quant.iter * 0.01)
            x.quant.prev <- x.quant.iter
            if(quant.iter < (1-prob[p])) x.quant.iter <- x.quant.iter - quant.iter.diff
            if(quant.iter >= (1-prob[p])) x.quant.iter <- x.quant.iter + quant.iter.diff
          }
        }
      } # End else loop
      x.quant <- c(x.quant, x.quant.prev)
  } # End prob loop
  x.quant <- round(x.quant)
  names(x.quant) <- prob
  return(x.quant)
}
