#' NWOS Plot Response Rates for Replicates
#'
#' This function calculates response rates for NWOS replicates and is designed to be used with an apply function, such as sapply.
#' @usage nwos_response_rate(index.rep, index, stratum, response)
#' @param index.rep list of observations (i.e., replicates) to include.
#' @param index vector used to identify the location of values in the other vectors (e.g., row names).
#' @param stratum vector indicating inclusion (1) and exclusion (0) in the stratum of interest. NA's are allowed.
#' @param response vector indicating response (1) and non-response (0).
#' @return
#' Response rate in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- tbl_df(read.csv("data/wi.csv")) %>% mutate(ROW_NAME = row.names(wi), AC_WOOD = ACRES_FOREST, FFO = if_else(LAND_USE == 1 & OWN_CD == 45 & AC_WOOD >= 1, 1, 0), RESPONSE = if_else(RESPONSE_PROPENSITY >= 0.5, 1, 0), RESPONSE = if_else(is.na(RESPONSE_PROPENSITY), 0, RESPONSE))
#' WI_FFO_RR_REP <- sapply(WI_REPLICATES, nwos_response_rate_apply, index = wi$ROW_NAME, stratum = wi$FFO, response = wi$RESPONSE)
#' WI_FFO_RR_REP

nwos_plot_response_rate_replicates <- function(index.rep, index, stratum, response) {
  index.rep <- unlist(index.rep)
  nwos_plot_response_rate(stratum = stratum[match(index.rep, index)],
                     response = response[match(index.rep, index)])
}

