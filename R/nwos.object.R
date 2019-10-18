#' nwos.object
#'
#' An S4 class to contain NWOS raw data, including metadata,weights,and imputations.  
#'
#' @slot quest is a data.frame containing raw questionaire data
#' @slot sample is a data.frame containing imformation on the sample
#' @slot metadata is a data.frame containing question metadata
#' @slot fields is a data.frame containing metadata for columns in sample and quest
#' @slot weight is a data.frame containing survey weights
#' @slot imputation is a data.frame containing imputation sets
#'
#' @export

nwos.object <- setClass("nwos.object",representation(quest='data.frame',
                                        sample='data.frame',
                                        metadata='data.frame',
                                        fields='data.frame',
                                        weights='data.frame',
										imputations='data.frame'))