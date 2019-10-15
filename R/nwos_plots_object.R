#' nwos_plots_object
#'
#' Creates an object of S4 class 'nwos.plots' 
#'
#' @param x is a list
#'
#' @return a nwos.plots.object
#'
#' @examples
#' nwos_plots_object(foo)
#'
#' @export

nwos_plots_object <- function(x){
  register_nwos_plots_object()
  npo <- new("nwos.plots.object",plots=x[[1]],response_codes=x[[2]])
  return(npo)
}