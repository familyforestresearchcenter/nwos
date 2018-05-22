#' NWOS Data
#'
#' This function creates a dataset that can be used to test the NWOS functions.
#' @usage nwosData()
#' @return
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' NWOS_DATA <- nwosData()
#' save(NWOS_DATA, file="data/nwos_data.RData")

nwosData <- function()
{
  A <- 1000
  a <- A / 2 # 500 1
  a <- c(a, rep(((A-a) / 2) / 5, 5)) # 50 5
  a <- c(a, rep(((A-sum(a)) / 2) / 25, 25)) # 5 25
  a <- c(a, rep(((A-sum(a)) / 1) / 125, 125)) # 1 125

  df <- data.frame(STATE=rep(0, NROW(a)),
                   STRATUM=rep(1, NROW(a)),
                   OWN_ID=1:NROW(a),
                   ACRES_FOREST=a,
                   RESPONSE=rep(1, NROW(a)),
                   Y_1=rbinom(NROW(a), 1, 0.5),
                   Y_2=rnorm(NROW(a), 100, 10))
  return(df)
}
