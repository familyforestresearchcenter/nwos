#' Add MINORITY Variable to NWOS Dataset
#'
#' Add variables to an NWOS dataframe
#' @usage nwos_estimates_data_add_variables(data = QUEST_WIDE, meta.data = QUEST_META, box2 = c(names(data)[grepl("OBJ_", names(data))], names(data)[grepl("CNC_", names(data))], "ATT_WOODED", "ATT_SELL", "KNOW_WOOD", "WANT_KNOW_WOOD", "EMO_WOOD", "WOOD_COMMUNITY"))
#' @param data = QUEST_WIDE
#' @param minority Logical value indicating if the OWN1_MINORITY variable should be created. Default = F
#' @param tele Logical value indicating if the TELE related variables should be created. Default = F
#' @keywords nwos
#' @details
#' The default values create the variables used in the NWOS tables.
#' @export
#' @examples
#' nwos_estimates_data_add_variables(minority = T, tele = T)

nwos_estimates_add_minority <- function(data = QUEST_WIDE) {
  require(tidyverse)

  data <- data %>%
    mutate(OWN1_MINORITY = factor(if_else((OWN1_ETH == 1 | OWN1_RACE_ASIAN == 1 | OWN1_RACE_BLACK == 1 |
                                             OWN1_RACE_HAWAIIAN == 1 | OWN1_RACE_INDIAN == 1), 1,
                                          if_else((OWN1_ETH == -1 | OWN1_RACE_ASIAN == -1), -1,
                                                  if_else((OWN1_ETH == -2 | OWN1_RACE_ASIAN == -2), -2, 0)))))
}
