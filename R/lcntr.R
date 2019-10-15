#' lcntr
#'
#' Prints a progres indicator for if-then loops
#'
#' @param x is the vector that is being looped through
#' @param counter is the sequence variable used in the counter
#'
#' @return a character value
#'
#' @examples
#' foo <- 1:100000
#' if (i in 1:length(foo)){
#' 		lcntr(foo,counter=i)
#' }
#'
#' @export

lcntr <- function(x,counter=i) {
	if (counter%%(length(x)/10) <= (counter-1)%%(length(x)/10) | counter==length(x)){
   	 message(paste(" ... ",floor(counter/length(x)*10),"0%",sep=""),appendLF=F)
  	}
  }