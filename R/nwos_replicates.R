#' NWOS Replicates
#'
#' Create a matrix of This function calculates variances for NWOS statistics using a bootstrapping approach.
#' @usage nwos_replicates(df, R=5000)
#' @param df data frame of survey data.
#' @param R number of replicates or bootstraps. Default is 5000.
#' @details
#' This function needs to be run by statev
#' @return
#' Returns a list containing the original data frame with an INDEX appended and a matrix of replicates.
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @keywords nwos
#' @export
#' @examples
#' load("../../ANALYSIS/SAMPLE_SIZE/DATA/WI_SAMPLE_SIZE_SAMPLES_20180721.RData")
#' df <- SAMPLE_SIZE_SAMPLES[SAMPLE_SIZE_SAMPLES$STATE_CD == 55 & SAMPLE_SIZE_SAMPLES$SAMPLE==1 & SAMPLE_SIZE_SAMPLES$SIZE==1000, ]
#' df.replicates <- nwos_replicates(df)
#' save(df.replicates, file = "data/DF_REPLICATES.RData")

nwos_replicates <- function(index, point.count, R = 5000) {
  # df <- df[rep(row.names(df), df$POINT_COUNT), ] # Expand df based on POINT_COUNT
  # df$POINT_COUNT <- 1 # Reset point counts to 1
  # df <- cbind(INDEX = 1:NROW(df), df) # Add index
  replicates <- matrix(nrow = sum(point.count), ncol = R, dimnames = list(1:sum(point.count), 1:R)) # Create empty matrix
  for(r in 1:R) {
    replicates[,r] <- sample(index, sum(point.count), replace = T, prob = point.count) # Generate replicates, indicated by INDEX
  }
  # return(list(df = df, replicates = replicates)) # Return matrix of replicates
  return(replicates) # Return matrix of replicates
}
