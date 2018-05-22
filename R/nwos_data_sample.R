#' NWOS Data Sample
#'
#' This function generates one or more samples of NWOS data using a probability proportional to size, with replacement, design.
#' @usage nwosDataSample(df,n,s)
#' @param df dataframe with NWOS structure (see Description file)
#' @param n number of samples per sample iteration
#' @param s number of sample iterations
#' @return Dataframe for sample
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data.RData")
#' NWOS_DATA_SAMPLE <- nwosDataSample(NWOS_DATA, n=25, s=1000)
#' save(NWOS_DATA_SAMPLE, file="data/nwos_data_sample.RData")

nwosDataSample <- function(df=NWOS_DATA, n=25, s=1)
{
  df.sample <- cbind(df[0,], POINT_COUNT=numeric(0), SAMPLE=numeric(0))

  for(i in 1:s)
  {
    df.sample.i <- df[sample(df$OWN_ID, n, T, df$ACRES_FOREST),]
    point.count <- as.data.frame(table(df.sample.i$OWN_ID))
    names(point.count) <- c("OWN_ID", "POINT_COUNT")
    df.sample.i <- unique(merge(df.sample.i, point.count))
    df.sample <- data.frame(rbind(df.sample,
                                  data.frame(df.sample.i,
                                             SAMPLE=i)))
  }
  row.names(df.sample) <- 1:NROW(df.sample)
  df.sample <- df.sample[,c("SAMPLE","STATE","STRATUM","OWN_ID","ACRES_FOREST","POINT_COUNT","RESPONSE", "Y_1", "Y_2")]

  return(df.sample)
}
