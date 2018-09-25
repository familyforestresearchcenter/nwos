#' NWOS Replicates
#'
#' Generate replicates that can be used for NWOS bootstrapping variance estimation.
#' @usage nwos_replicates(index = NA, data, point.count = "POINT_COUNT", R = 5000)
#' @param index vector of names used to identify rows in data. If NA, the index is set to the row names of data.
#' @param data data frame containing stratum and point.count variables.
#' @param point.count the name of a variable in data indicating the number of sample points associated with each observation.
#' @param R number of replicates or bootstraps. Default is 5000.
#' @details
#' This function needs to be run by state.
#' @return
#' List containing indices for the observations that make up each of the replicates.
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @keywords nwos
#' @export
#' @examples
#' REPLICATES <- nwos_replicates(index = row.names(wi), data = wi,  point.count = "POINT_COUNT", R = 100)

nwos_replicates <- function(index = NA, data, point.count = "POINT_COUNT", R = 5000) {
  if(is.na(index[1])) index <- row.names(data)
  if(point.count == 1) x <- data.frame(index, 1)
  else x <- data.frame(index, data[, point.count]) # Create data frame
  names(x) <- c("index", "point.count")
  # data <- data.frame(index, point.count, stringsAsFactors = FALSE) %>%
  #   slice(rep(1:n(), point.count)) %>% # Expand data based on POINT_COUNT
  #   mutate(point.count = 1) # Reset point counts to 1
  x <- x %>%
    slice(rep(1:n(), point.count)) %>% # Expand data based on POINT_COUNT
    mutate(point.count = 1) # Reset point counts to 1
  replicates <- replicate(R, sample(x$index, NROW(x$index), replace = T)) # Generate replicates
  split(replicates, col(replicates)) # Convert to list
}
