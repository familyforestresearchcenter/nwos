#' Add Variables to NWOS QUEST objects
#'
#' Add variables to an NWOS data frame
#' @usage nwos_estimates_add_variable(x = 1, data = QUEST_LIST, variable.name = "TEN_PLUS", variable.definition = "AC_WOOD >= 10")
#' @param x list number. Only applicable if is data is a list of data frames, instead of a single data frame. This used mainly for apply functions.
#' @param data list of NWOS QUEST objects
#' @keywords nwos
#' @details
#' Add a new variable to all NWOS QUEST objects in a list.
#' @export
#' @examples
#' nwos_estimates_add_variable()

nwos_estimates_add_variable <- function (x = 1,
                                         data = QUEST_LIST,
                                         variable.name = "TEN_PLUS",
                                         variable.definition = "AC_WOOD >= 10")
{
  require(rlang)
  require(tidyverse)
  data[[x]] %>%
    mutate(!!variable.name :=
             !!parse_quo(paste0("if_else(", variable.definition, ", T, F)"),
                         env = caller_env()))
}
