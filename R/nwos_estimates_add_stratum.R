#' Add Stratum Variables to an NWOS Data Set
#'
#' Add variables to an NWOS data frame
#' @usage nwos_estimates_add_stratum(x = NA, data = QUEST)
#' @param x list number. Only applicable if is data is a list of data frames, instead of a single data frame. This used mainly for apply functions.
#' @param data data frame or list of data frames
#' @keywords nwos
#' @details
#' The default values create the variables used in the NWOS tables.
#' @export
#' @examples
#' nwos_estimates_add_stratum()

nwos_estimates_add_stratum <- function(x = NA, data = QUEST, stratum = c("FFO")) {
  require(tidyverse)
  if(!is.data.frame(data)) data <- data[[x]]

  if("FFO" %in% stratum)
    data <- data %>% mutate(FFO = if_else(COND_STATUS_CD %in% 1 & OWNCD_NWOS %in% 45, 1, 0))

  return(data)
}
