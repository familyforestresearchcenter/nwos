#' nwos_estimates_make_quest
#'
#' Convert nwos.object to wide tables - separate tables for each umputation
#'
#' @param geo.cd.list,
#' @param imp
#' @param quest = QUEST
#' @param variables.categorcial = VARIABLES_CATEGORICAL,
#' @param variables.continuous = VARIABLES_CONTINUOUS)
#'
#' @return tibble
#'
#' @examples
#' nwos_estimates_make_quest()
#'
#' @export

nwos_estimates_make_quest <- function(imp = 1, quest = QUEST, own = 45,
                                      variables.categorcial = VARIABLES_CATEGORICAL,
                                      variables.continuous = VARIABLES_CONTINUOUS) {
  as_tibble(nwos_data_recode(nwos_full(quest, imputations = imp),
                   nwos_wide_metadata(quest) %>% filter(!ITEM_TYPE %in% 4)) %>%
    mutate(COND_STATUS_CD = if_else(is.na(COND_STATUS_CD), -1, as.numeric(COND_STATUS_CD)),
           OWNCD_NWOS  = if_else(is.na(OWNCD_NWOS), -1, as.numeric(OWNCD_NWOS)),
           RESP = if_else(OWNCD_NWOS %in% own & !is.na(CN), 1, 0),
           PROB = 1 / FINAL_WEIGHT,
           PROB_AC = 1 / (FINAL_WEIGHT * AC_WOOD),
           ONE = 1) %>%
    select(CN, STATECD_NWOS, COND_STATUS_CD, OWNCD_NWOS, RESP, PROB, PROB_AC, ONE,
           unique(variables.categorcial$VARIABLE), variables.continuous))
}
