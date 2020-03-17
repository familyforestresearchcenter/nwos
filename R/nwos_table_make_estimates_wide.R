#' nwos_table_set
#'
#' Create the body of an NWOS table
#' @param GEO
#' @param TAB_NUM
#' @param TAB_TYPE table type. AREA = area by ownership category. COOP = cooperation rate. QUEST (Default) = questionnaire content.
#' @param POP . Default = "Family".
#' @param DOMAIN = NA
#' @param TABLE
#' @param YEARS Default = "2017-2018"
#' @details For area and cooperation rate tables see ...
#' nwos_table_body()
#' nwos_table_body_area()
#' nwos_table_body_coop()
#' @examples 
#' nwos_table_make_estimates_wide(quest.file = "INPUTS/ESTIMATES/NWOS_2018_FFO_ESTIMATES_20200221.RDS", 
#' domain = "AC_WOOD >= 1")
#' 

nwos_table_make_estimates_wide <- function(estimates = QUEST_EST, domain) {
  full_join(estimates %>% 
              filter(UNITS %in% "ACRES", STATISTIC %in% "TOTAL") %>%
              rename(AC = VALUE, AC_VAR = VARIANCE) %>% select(-STATISTIC, -UNITS),
            estimates %>% 
              filter(UNITS %in% "OWNERSHIPS", STATISTIC %in% "TOTAL") %>%
              rename(OWN = VALUE, OWN_VAR = VARIANCE) %>% select(-STATISTIC, -UNITS),
            by = c("GEO_ABB", "DOMAIN_ABB", "OWN_ABB", "VARIABLE", "LEVEL")) %>%
    full_join(estimates %>% 
                filter(UNITS %in% "ACRES", STATISTIC %in% "PROPORTION") %>%
                rename(AC_PROP = VALUE, AC_PROP_VAR = VARIANCE) %>% select(-STATISTIC, -UNITS),
              by = c("GEO_ABB", "DOMAIN_ABB", "OWN_ABB", "VARIABLE", "LEVEL")) %>%
    full_join(estimates %>% 
                filter(UNITS %in% "OWNERSHIPS", STATISTIC %in% "PROPORTION") %>%
                rename(OWN_PROP = VALUE, OWN_PROP_VAR = VARIANCE) %>% select(-STATISTIC, -UNITS),
              by = c("GEO_ABB", "DOMAIN_ABB", "OWN_ABB", "VARIABLE", "LEVEL")) %>%
    full_join(estimates %>% 
                filter(STATISTIC %in% "N") %>%
                rename(N = VALUE) %>% select(-STATISTIC, -UNITS, -VARIANCE),
              by = c("GEO_ABB", "DOMAIN_ABB", "OWN_ABB", "VARIABLE", "LEVEL")) %>%
    arrange(GEO_ABB, DOMAIN_ABB, OWN_ABB, VARIABLE, LEVEL)
}

