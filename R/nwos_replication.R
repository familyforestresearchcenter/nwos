#' NWOS Replication
#'
#' This function replicates the rows of a dataframe based on plot count.
#' @usage nwosMean(weight, point.count, domain, y=1, area, units="ownerships")
#' @param df is a data frame
#' @param r is the number of replicates for each observation
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.

nwosReplication <- function(df, r)
{
  df <- df[rep(row.names(df),r),]
  return(df)
}
