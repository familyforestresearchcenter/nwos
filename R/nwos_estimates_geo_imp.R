#' nwos_estimates_geo_imp
#'
#' Generate estimates by geography and imputation
#' This is functuion is designed to be called by nwos_estimates_geo
#'
#' @param geo.cd.list,
#' @param imp,
#' @param own = 45,
#' @param domain,
#' @param quest = QUEST,
#' @param quest.meta = QUEST_META,
#' @param plot = PLOTS,
#' @param variables.categorcial = VARIABLES_CATEGORICAL,
#' @param variables.continuous = VARIABLES_CONTINUOUS
#'
#' @return data frame
#'
#' @examples
#' nwos_estimates_geo_imp()
#'
#' @export

nwos_estimates_geo_imp <- function(geo.cd.list,
                                   imp,
                                   own = 45,
                                   domain,
                                   quest = QUEST,
                                   quest.meta = QUEST_META,
                                   plot = PLOTS,
                                   variables.categorcial = VARIABLES_CATEGORICAL,
                                   variables.continuous = VARIABLES_CONTINUOUS) {
  options(warn=-1)
  if(grepl(", ", geo.cd.list)) geo.cd <- unlist(strsplit(geo.cd.list, split=", "))
  else geo.cd <- geo.cd.list
  quest.wide <- as_tibble(nwos_wide(quest, imputations = imp)) %>%
    filter(STATECD_NWOS %in% geo.cd, OWNCD_NWOS %in% own)
  quest.wide <- nwos_data_recode(quest.wide, quest.meta) %>%
    mutate(PROB = 1 / FINAL_WEIGHT,
           PROB_AC = 1 / (FINAL_WEIGHT * AC_WOOD),
           ONE = 1) %>%
    select(RESPONSE_CN = CN, PROB, PROB_AC, ONE,
           unique(variables.categorcial$VARIABLE), variables.continuous) # AC_WOOD, OWNTYPE, MAN_PLAN)  #

  plot <- plot %>% filter(STATECD_NWOS %in% geo.cd)

  plot.quest <<- left_join(plot, quest.wide, by = "RESPONSE_CN") %>%
    mutate(COND_STATUS_CD = if_else(is.na(COND_STATUS_CD), -1, as.numeric(COND_STATUS_CD)),
           OWNCD_NWOS  = if_else(is.na(OWNCD_NWOS), -1, as.numeric(OWNCD_NWOS)),
           RESP = if_else(OWNCD_NWOS %in% own & RESPONSE_CAT %in% "I", 1, 0))

  design.own <<- twophase(id = list(~ 1, ~ RESPONSE_CN),
                         strata = list(~ STATECD_NWOS + COND_STATUS_CD + OWNCD_NWOS, NULL), # STRATUM
                         subset = ~ RESP,
                         probs = list(NULL, ~ PROB),
                         data = plot.quest)
  design.ac <<-  twophase(id = list(~ 1, ~ RESPONSE_CN),
                         strata = list(~ STATECD_NWOS + COND_STATUS_CD + OWNCD_NWOS, NULL),
                         subset = ~ RESP,
                         probs = list(NULL, ~ PROB_AC),
                         data = plot.quest)

  bind_rows(do.call(rbind, mapply(variable.name = variables.categorcial$VARIABLE,
                                  level.name = variables.categorcial$LEVEL,
                                  nwos_estimates_categorical,
                                  SIMPLIFY = F)),
            do.call(rbind, mapply(variable.name = variables.continuous,
                                  nwos_estimates_continuous,
                                  SIMPLIFY = F))) %>%
    mutate(IMPUTATION = imp, GEO_CD = paste(geo.cd, collapse = ", ")) %>% select(GEO_CD, everything())
}
