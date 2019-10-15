#' nwos_object
#'
#' Creates an object of S4 class 'nwos.object' 
#'
#' @param x is a list
#'
#' @return a nwos.object
#'
#' @examples
#' nwos_object(foo)
#'
#' @export

nwos_object <- function(x){
  register_nwos_object()
  nwo <- new("nwos.object",quest=x[[1]],
	sample=x[[2]],
	metadata=x[[3]],
	fields=x[[4]],
	weights=x[[5]],
	imputations=x[[6]])
  return(nwo)
}