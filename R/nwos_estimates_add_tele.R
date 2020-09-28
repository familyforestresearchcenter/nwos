#' Add TELE Variables to NWOS Dataset
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

nwos_estimates_add_tele <- function(data = QUEST_WIDE) {
  require(tidyverse)

  # Attitudinal
  tele.att <- data %>%
    select(OBJ_TIM, OBJ_BEA, OBJ_NAT, OBJ_PRI, OBJ_HUNT, OBJ_REC) %>%
    mutate(across(OBJ_TIM, .fns = function(x) if_else(x %in% 4:5, 1, 0))) %>%
    mutate(across(-OBJ_TIM, .fns = function(x) if_else(x == 5, 1, 0))) %>%
    mutate(TELE_ATT_FINANCIAL = as.factor(OBJ_TIM),
           TELE_ATT_AMENITY = as.factor(if_else(OBJ_BEA == 1 | OBJ_NAT == 1 | OBJ_PRI == 1 |
                                                  OBJ_HUNT == 1 | OBJ_REC == 1,
                                                1, 0)))

  tele.att.mat <- tibble(TELE_ATT_FINANCIAL = as.factor(c(0, 1, 1, 0)),
                         TELE_ATT_AMENITY = as.factor(c(1, 1, 0, 0)),
                         TELE_ATTITUDINAL = as.factor(c(1, 2, 3, 4)))
  # 1= Woodland retreat; 2 = Workign the land; 3 = Supplemental income; 4 = Uninvolved

  tele.att <- tele.att %>%
    left_join(tele.att.mat, by = c("TELE_ATT_FINANCIAL", "TELE_ATT_AMENITY")) %>%
    select(TELE_ATT_FINANCIAL, TELE_ATT_AMENITY, TELE_ATTITUDINAL)

  # Prime Prospects
  tele.prime.att <- data %>%
    select(OBJ_NAT, OBJ_WAT, OBJ_WIL, CNC_HEIR, ATT_WOODED) %>%
    mutate(across(.fns = function(x) if_else(x %in% 4:5, 1, 0))) %>%
    mutate(TELE_PRIME_ATTITUDE = rowSums(.)) %>%
    mutate(TELE_PRIME_ATTITUDE_CAT = cut(TELE_PRIME_ATTITUDE, c(0, 1, 4, Inf), labels = 0:2, include.lowest = T, right = F))

  tele.prime.beh <- data %>%
    mutate(ADVICE_PRO = if_else(ADV_SRC_STATE==1 | ADV_SRC_FED==1 | ADV_SRC_PRIV==1, 1, 0)) %>%
    select(MAN_PLAN_IMPLEMENT, CUT_FORESTER, CERT, COST_5YR, EASE,
           ACT_RED_FIRE, ACT_CONTROL_BURN, ACT_INVA, ACT_INS, ACT_WILD, ADVICE_PRO) %>%
    mutate(across(.fns = function(x) if_else(x == 1, 1, 0))) %>%
    mutate(TELE_PRIME_BEHAVIOR =rowSums(.)) %>%
    mutate(TELE_PRIME_BEHVAIOR_CAT = cut(TELE_PRIME_BEHAVIOR, c(0, 1, 4, Inf), labels = 0:2, include.lowest = T, right = F))

  tele.prime.mat <- tibble(TELE_PRIME_ATTITUDE_CAT = as.factor(c(2, 2, 1, 1, 2, 1, 0, 0, 0)),
                           TELE_PRIME_BEHVAIOR_CAT = as.factor(c(2, 1, 1, 0, 0, 2, 2, 1, 0)),
                           TELE_PRIME = as.factor(c(1, 2, 2, 2, 2, 3, 3, 3, 4))) # 1= Model; 2 = Prime; 3 = Defector; 4 = Write-off

  tele.prime <- bind_cols(tele.prime.att, tele.prime.beh) %>%
    left_join(tele.prime.mat, by = c("TELE_PRIME_ATTITUDE_CAT", "TELE_PRIME_BEHVAIOR_CAT")) %>%
    select(TELE_PRIME_ATTITUDE, TELE_PRIME_ATTITUDE_CAT, TELE_PRIME_BEHAVIOR, TELE_PRIME_BEHVAIOR_CAT, TELE_PRIME)

  # Engagement
  tele.eng <- data %>%
    select(ACT_CUT_SALE, ACT_CUT_PERS, ACT_NTFP, ACT_RED_FIRE, ACT_CONTROL_BURN, ACT_INVA,
           ACT_INS, ACT_ROAD, ACT_TRAIL, ACT_WILD, ACT_GRAZE, ACT_OTH) %>%
    mutate(across(.fns = as.character)) %>%
    mutate(across(.fns = as.numeric)) %>%
    mutate(TELE_ACT_COUNT =rowSums(.)) %>%
    mutate(TELE_ENGAGEMENT = cut(TELE_ACT_COUNT, c(0, 1, 2, 4, Inf), labels = 0:3, include.lowest = T, right = F)) %>%
    select(TELE_ACT_COUNT, TELE_ENGAGEMENT)

  # Combine
  tele <- bind_cols(tele.att, tele.prime, tele.eng) %>%
    select(TELE_ATT_FINANCIAL, TELE_ATT_AMENITY, TELE_ATTITUDINAL,
           TELE_PRIME_ATTITUDE, TELE_PRIME_ATTITUDE_CAT, TELE_PRIME_BEHAVIOR, TELE_PRIME_BEHVAIOR_CAT, TELE_PRIME,
           TELE_ACT_COUNT, TELE_ENGAGEMENT)

  data %>% bind_cols(tele)
}
