#' nwos.plots.object
#'
#' An S4 class to contain NWOS raw plot- and sample-level data  
#'
#' @slot plots is a data.frame containing raw plot-level data for a cycle
#' @slot response_codes is a data.frame containing response codes and other sample-level data to attach to plots
#'
#' @export

setClass("nwos.plots.object",representation(plots='data.frame',response_codes='data.frame'))