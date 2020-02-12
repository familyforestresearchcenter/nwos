#' nwos.object
#'
#' An S4 class to contain NWOS raw data, including metadata,weights,and imputations.  
#'
#' @slot quest is a data.frame containing raw questionaire data
#' @slot sample is a data.frame containing imformation on the sample
#' @slot metadata is a data.frame containing question metadata
#' @slot fields is a data.frame containing metadata for columns in sample and quest
#' @slot weights is a data.frame containing survey weights
#' @slot imputations is a data.frame containing imputation sets
#' @slot plots is a data.frame containing the full (ie. plot-level) sample
#'
#' @export

nwos.object <- setClass("nwos.object",representation(quest='data.frame',
                                        sample='data.frame',
                                        metadata='data.frame',
                                        fields='data.frame',
                                        weights='data.frame',
										imputations='data.frame',
										plots='data.frame'))