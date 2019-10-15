#' register_nwos_plots_object
#'
#' Registers S4 class 'nwos.plots' in working environment 
#'
#' @examples
#' register_nwos_plots_object(foo)
#'
#' @export

register_nwos_plots_object <- function(){
  setClass("nwos.plots.object",representation(plots='data.frame',
                                        response_codes='data.frame'))
}