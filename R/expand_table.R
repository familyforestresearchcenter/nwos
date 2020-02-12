#' expand_table
#'
#' A function for replicating each record in a dataframe by an expansion factor stored as a separate column in that dataframe
#'
#' @param x is a nwos.object
#' @param col is string referencing the numeric column containing record-level expansion factors
#' 
#' @return a data.frame
#'
#' @examples
#' expand_table(foo,col='NUM_PLOTS')
#'
#' @export

expand_table <- function(x,col){
  df <- x[rep(row.names(x),x[,col]),]
  return(df)
}