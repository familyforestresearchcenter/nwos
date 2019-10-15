#' register_nwos_object
#'
#' Registers S4 class 'nwos.object' in working environment 
#'
#' @examples
#' register_nwos_object(foo)
#'
#' @export

register_nwos_object <- function(){
  setClass("nwos.object",representation(quest='data.frame',
                                        sample='data.frame',
                                        metadata='data.frame',
                                        fields='data.frame',
                                        weights='data.frame',
										imputations='data.frame'))
}