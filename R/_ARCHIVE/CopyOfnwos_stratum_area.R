#' NWOS Stratum Area
#'
#' This function estimates NWOS stratum areas.
#' @usage nwos_stratum_area(index = NA, data, stratum = "FFO", point.count = "POINT_COUNT", state.area)
#' @param index vector of observations in data to include. If NA, the index is set to the row names of data (i.e., uses all of the rows in data)
#' @param data data frame containing stratum and point.count variables.
#' @param stratum the name of a variable in data indicating inclusion (1) and exclusion (0) in the stratum of interest.
#' @param point.count the name of a variable in data indicating the number of sample points associated with each observation.
#' @param state.area total land area in the state of interest.
#' @details
#' This function needs to be run by state.
#' @return
#' Area of forestland in the stratum.
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' wi <- wi %>% mutate(FFO = if_else(LAND_USE == 1 & OWN_CD == 45, 1, 0))
#' WI_FFO_AREA <- nwos_stratum_area(data = wi, stratum = "FFO", state.area = 33898733)
#' WI_FFO_AREA

nwos_stratum_area <- function(index = NA, data, stratum = "FFO", point.count = "POINT_COUNT", state.area) {
  if(is.na(index[1])) index <- row.names(data)
  if(point.count == 1) x <- data.frame(data[index, stratum], 1)
  else x <- data.frame(data[index, stratum], data[index, point.count]) # Create data frame
  names(x) <- c("stratum", "point.count")
  n <- x %>% filter(stratum %in% c(0,1)) %>% summarize(sum(point.count)) # Number of sample points
  n.s <- x %>% filter(stratum %in% c(1)) %>% summarize(sum(point.count)) # Number of sample points in stratum
  as.numeric((n.s / n) * state.area) # Calculate stratum area
}
