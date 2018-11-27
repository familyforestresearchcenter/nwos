#' NWOS Replicates
#'
#' Generate replicates that can be used for NWOS bootstrapping variance estimation.
#' @usage nwos_replicates(index, point.count, R = 5000)
#' @param index vector of names used to identify rows/observations in the data.
#' @param point.count the number of sample points associated with each observation.
#' @param R number of replicates or bootstraps. Default is 2500.
#' @details
#' This function needs to be run by state.
#' @return
#' List containing indices for the observations that make up each of the replicates.
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @keywords nwos
#' @export
#' @examples
#' WI_REPLICATES <- nwos_replicates(index = row.names(wi), point.count = wi$POINT_COUNT, R = 100)

nwos_replicates <- function(index, point.count, R = 2500) {
  data <- tbl_df(list(index = index, point.count = point.count)) %>% # Combine index and point.count variables into a single data frame
    slice(rep(1:n(), point.count)) %>% # Expand data based on point.count
    mutate(point.count = 1) # Reset point counts to 1
  replicates <- replicate(R, sample(data$index, NROW(data$index), replace = T)) # Generate replicates
  split(replicates, col(replicates)) # Convert to list
}
