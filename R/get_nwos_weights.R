#' get_nwos_weights
#'
#' Downloads replicate weights for NWOS samples
#'
#' @details
#' This function must be run on a computer connected to the USFS T drive, with appropriate read permissions 
#'
#' @param cycle is a string containing the NWOS cycle desired.
#' @param strata is a string containing the name of the strata desired.
#'
#' @examples
#' get_nwos_weights(cycle='2018',strata='FFO')
#'
#' @export

get_nwos_weights <- function(cycle='2018',strata='FFO'){
  
  file <- 'T:/FS/RD/FIA/NWOS/ESTIMATION/DATA/NWOS_<YR>_<STRATA>/REP_WEIGHTS.RDS'
  file <- gsub('<YR>',cycle,file)
  file <- gsub('<STRATA>',strata,file)
  readRDS(file)
  
}
