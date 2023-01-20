#' cg_max
#'
#' Calculate maximum values for check group questions
#'
#' @details
#' For area and cooperation rate tables see ...
#'
#' @param check_group
#' @param quest_wide
#' @param quest_meta
#'
#' @examples
#' cg_max("ACQ_TYPE")
#'
#' @export

cg_max <- function(check_group = "ACQ_TYPE", quest_wide = QUEST_WIDE, quest_meta = QUEST_META){
  check_group_variables <- quest_meta %>%
    filter(CHECK_GROUP %in% check_group, ITEM_TYPE %in% 2) %>%
    pull(COLUMN)
  quest_wide %>%
    select(check_group_variables) %>%
    mutate(!!check_group := do.call(pmax, (.))) %>%
    pull(!!check_group)
}
