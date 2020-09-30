#' Add MINORITY Variable to an NWOS Data Set
#'
#' Add variables to an NWOS data frame
#' @usage nwos_estimates_add_minority(x = NA, data = QUEST)
#' @param x list number. Only applicable if is data is a list of data frames, instead of a single data frame. This used mainly for apply functions.
#' @param data data frame or list of data frames
#' @keywords nwos
#' @details
#' The default values create the variables used in the NWOS tables.
#' @export
#' @examples
#' nwos_estimates_add_minority()

nwos_estimates_add_minority <- function(x = NA, data = QUEST) {
  require(tidyverse)
  if(!is.data.frame(data)) data <- data[[x]]
  data %>%
    mutate(OWN1_MINORITY = factor(if_else((OWN1_ETH == 1 | OWN1_RACE_ASIAN == 1 | OWN1_RACE_BLACK == 1 |
                                             OWN1_RACE_HAWAIIAN == 1 | OWN1_RACE_INDIAN == 1), 1,
                                          if_else((OWN1_ETH == -1 | OWN1_RACE_ASIAN == -1), -1,
                                                  if_else((OWN1_ETH == -2 | OWN1_RACE_ASIAN == -2), -2, 0)))))
}
