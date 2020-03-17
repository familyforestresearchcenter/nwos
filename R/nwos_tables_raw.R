#' nwos_tables_raw
#'
#' Create the body of an NWOS core, area, or cooperataion rate table
#' @param data
#' @details For area and cooperation rate tables see ...
#' nwos_table_raw()

nwos_tables_raw <- function(area_data = NA, coop_data = NA,
                            quest_data = QUEST_EST, quest_tab_meta_data = REF_TABLE) {
  # Area
  # Coop
  # Quest 
  # !!Need to add proportions!!
  q <- left_join(quest_data %>%
                   filter(STATE %in% "1") %>% # For testing
                   filter(STATISTIC %in% c("TOTAL", "N")) %>%
                   select(-STATISTIC, -VARIANCE) %>%
                   spread(UNITS, VALUE),
                 quest_data %>%
                   filter(STATE %in% "1") %>% # For testing
                   filter(STATISTIC %in% c("TOTAL")) %>%
                   select(-STATISTIC, -VALUE) %>%
                   spread(UNITS, VARIANCE) %>%
                   rename("ACRES_VAR" = "ACRES", "OWNERSHIPS_VAR" = "OWNERSHIPS"),
                 by = c("STATE", "VARIABLE", "LEVEL")) %>%
    mutate(ACRES_SE = sqrt(ACRES_VAR),
           OWNERSHIPS_SE =  sqrt(OWNERSHIPS_VAR)) %>%
    mutate_at(vars(ACRES, ACRES_SE, OWNERSHIPS, OWNERSHIPS_SE), nwos_round) %>%
    select(FIPS = STATE, TABLE = VARIABLE, LEVEL, ACRES, ACRES_SE, OWNERSHIPS, OWNERSHIPS_SE, N = RESPONDENTS) %>%
    arrange(as.numeric(FIPS), TABLE, as.numeric(LEVEL))
  q
  q %>% filter(VARIABLE %in% "AC_WOOD_CAT") %>% distinct(LEVEL) %>% pull()
  # Update row names, levels, etc. (using meta data)
  quest_tab_meta_data <- quest_tab_meta_data %>% 
    select(TABLE, LEVELS, LABELS) 
  quest_tab_meta_data <- 
    do.call(rbind.data.frame, 
            lapply(1:NROW(quest_tab_meta_data), 
                   function(x) {tibble(TABLE = rep(quest_tab_meta_data$TABLE[x], 
                                                   length(unlist(strsplit(quest_tab_meta_data[x,]$LEVELS, ",")))),
                                       LEVEL = unlist(strsplit(quest_tab_meta_data[x,]$LEVELS, ",")),
                                       LABEL = unlist(strsplit(quest_tab_meta_data[x,]$LABELS, ",")))}))
  quest_tab_meta_data

  # Convert to long
  # LEVEL to LABEL
  q %>% left_join(quest_tab_meta_data) %>%
    # select(-LEVEL) %>%
    select(FIPS, TABLE, LEVEL, LABEL, everything())
  #geo - state_abb (?)
  return(q)
}