#' nwos_estimates_make_quest
#'
#' Convert nwos.object to wide tables - separate tables for each umputation
#'
#' @return wide quest data frame
#'
#' @examples
#' NWOS <- readRDS("/Volumes/DATA/NWOS_20200212.RDS")
#' VARIABLES_CATEGORICAL <- read.csv("DATA/VARIABLES_CATEGORICAL.csv")
#' VARIABLES_CONTINUOUS <- read.csv("DATA/VARIABLES_CONTINUOUS.csv")
#' nwos_estimates_make_quest()
#' QUEST <- lapply(1:5, nwos_estimates_make_quest)
#' saveRDS(QUEST, "~/Desktop/QUEST_20200212.RDS")
#'
#' @export

nwos_estimates_make_quest <- function(imp = 1, nwos = NWOS, own = 45,
                                      variables.categorcial = VARIABLES_CATEGORICAL,
                                      variables.continuous = VARIABLES_CONTINUOUS) {
  as_tibble(nwos_estimates_data_recode(nwos_wide(nwos, imputations = imp),
                             nwos_wide_metadata(nwos) %>% filter(!ITEM_TYPE %in% 4)) %>%
              mutate(COND_STATUS_CD = 1,
                     OWNCD_NWOS  = if_else(is.na(OWNCD_NWOS), -1, as.numeric(OWNCD_NWOS)),
                     RESP = if_else(OWNCD_NWOS %in% own & !is.na(CN), 1, 0),
                     WEIGHT = FINAL_WEIGHT * PLOT_COUNT) %>%
              select(CN, STATECD_NWOS, COND_STATUS_CD, OWNCD_NWOS, RESP, WEIGHT,
                     unique(as.character(variables.categorcial$VARIABLE)),
                     unique(as.character(variables.continuous$VARIABLE))))
}
