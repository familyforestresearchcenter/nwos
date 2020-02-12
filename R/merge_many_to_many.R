#' merge_many_to_many
#'
#' Merges two dataframes with a many to many relationship, by attempting to match each non-unique value in a specified column of one dataframe with no more than one non-unique value in a specified column of the second. This is useful where the frequency of non-unique values in the two columns is the same. The default behavior is a left join.
#'
#' @param x is a dataframe to be merged
#' @param y is a dataframe to be merged
#' @param by.x is the field of x to be used as a non-unique key
#' @param by.y is the field of y to be used as a non-unique key
#'
#' @return a dataframe
#'
#' @examples
#' merge_many_to_many(x="foobar",y="foobar",by.x="id",by.y="id")
#'
#' @export

merge_many_to_many <- function(x,y,by.x,by.y){
  y$M2MJOINKEY <- paste("ID",1:nrow(y)) #add unique key to y

  x <- x[order(x[by.x]),] #order
  num <- unlist(sapply(table(x[by.x]),function(z){seq(1,z)}))
  UKx <- paste(x[,by.x],num,sep="") #unique key

  y <- y[order(y[by.y]),] #order
  num <- unlist(sapply(table(y[by.y]),function(z){seq(1,z)}))
  UKy <- paste(y[,by.y],num,sep="") #unique key

  x$M2MJOINKEY <- y$M2MJOINKEY[match(UKx,UKy)] #add merge key based on unique keys
  df <- merge(x,y,by="M2MJOINKEY",all.x=T)
  df <- df[,names(df)[names(df)!='M2MJOINKEY']]
  return(df)
}
