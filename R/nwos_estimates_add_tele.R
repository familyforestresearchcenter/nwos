# Add TELE Variables to NWOS Dataset
# x = QUEST_LIST index numbers
# data = QUEST_LIST
# TELE_ATT: 1= Woodland retreat; 2 = Workign the land; 3 = Supplemental income; 4 = Uninvolved
# TELE_PRIME: 1= Model; 2 = Prime; 3 = Defector; 4 = Write-off

add_tele <- function(x = NA, data = QUEST_LIST) {
  require(tidyverse)
  
  if(!is.data.frame(data)) data <- data[[x]]
  
  # Attitudinal
  tele.att <- data %>%
    mutate(TELE_ATT_FINANCIAL = factor(if_else((OBJ_TIM %in% 4:5 | 
                                                  OBJ_INV %in% 4:5), 1, 0)),
           TELE_ATT_AMENITY = factor(if_else((OBJ_BEA == 5 | OBJ_NAT == 5 | OBJ_PRI == 5 |
                                                OBJ_HUNT == 5 |OBJ_REC == 5), 1, 0)),
           TELE_ATT = factor(case_when(TELE_ATT_FINANCIAL == 0 & TELE_ATT_AMENITY == 1 ~ 1, # 1 = Woodland retreat
                                       TELE_ATT_FINANCIAL == 1 & TELE_ATT_AMENITY == 1 ~ 2, # 2 = Workign the land
                                       TELE_ATT_FINANCIAL == 1 & TELE_ATT_AMENITY == 0 ~ 3, # 3 = Supplemental income
                                       TELE_ATT_FINANCIAL == 0 & TELE_ATT_AMENITY == 0 ~ 4))) # 4 = Uninvolved
  
  # Prime Prospects
  tele.prime.att <- data %>%
    select(OBJ_NAT, OBJ_WAT, OBJ_WIL, CNC_HEIR, ATT_WOODED) %>%
    # mutate(across(.fns = function(x) if_else(x %in% 4:5, 1, 0))) %>%
    mutate_all(.funs = function(x) if_else(x %in% 4:5, 1, 0)) %>%
    mutate(TELE_PRIME_ATTITUDE = rowSums(.)) %>%
    mutate(TELE_PRIME_ATTITUDE_CAT = factor(case_when(TELE_PRIME_ATTITUDE == 0 ~ 0,
                                                      TELE_PRIME_ATTITUDE %in% 1:3 ~ 1,
                                                      TELE_PRIME_ATTITUDE >= 4 ~ 2)))
  
  tele.prime.beh <- data %>%
    mutate(ADVICE_PRO = if_else(ADV_SRC_STATE==1 | ADV_SRC_FED==1 | ADV_SRC_PRIV==1, 1, 0)) %>%
    select(MAN_PLAN_IMPLEMENT, CUT_FORESTER, CERT, COST_5YR, EASE,
           ACT_RED_FIRE, ACT_CONTROL_BURN, ACT_INVA, ACT_INS, ACT_WILD, ADVICE_PRO) %>%
    # mutate(across(.fns = function(x) if_else(x == 1, 1, 0))) %>%
    mutate_all(.funs = function(x) if_else(x == 1, 1, 0)) %>%
    mutate(TELE_PRIME_BEHAVIOR =rowSums(.)) %>%
    mutate(TELE_PRIME_BEHVAIOR_CAT = factor(case_when(TELE_PRIME_BEHAVIOR == 0 ~ 0,
                                                      TELE_PRIME_BEHAVIOR %in% 1:3 ~ 1,
                                                      TELE_PRIME_BEHAVIOR >= 4 ~ 2)))
  
  tele.prime <- bind_cols(tele.prime.att, tele.prime.beh) %>%
    mutate(TELE_PRIME = factor(case_when(TELE_PRIME_ATTITUDE_CAT == 2 & TELE_PRIME_BEHVAIOR_CAT == 2 ~ 1, # 1 = Model
                                         TELE_PRIME_ATTITUDE_CAT == 2 & TELE_PRIME_BEHVAIOR_CAT == 1 ~ 2, # 2 = Prime
                                         TELE_PRIME_ATTITUDE_CAT == 2 & TELE_PRIME_BEHVAIOR_CAT == 0 ~ 2,
                                         TELE_PRIME_ATTITUDE_CAT == 1 & TELE_PRIME_BEHVAIOR_CAT == 2 ~ 2,
                                         TELE_PRIME_ATTITUDE_CAT == 1 & TELE_PRIME_BEHVAIOR_CAT == 1 ~ 2,
                                         TELE_PRIME_ATTITUDE_CAT == 1 & TELE_PRIME_BEHVAIOR_CAT == 0 ~ 2,
                                         TELE_PRIME_ATTITUDE_CAT == 0 & TELE_PRIME_BEHVAIOR_CAT == 2 ~ 3, # 3 = Defector
                                         TELE_PRIME_ATTITUDE_CAT == 0 & TELE_PRIME_BEHVAIOR_CAT == 1 ~ 3,
                                         TELE_PRIME_ATTITUDE_CAT == 0 & TELE_PRIME_BEHVAIOR_CAT == 0 ~ 4))) # 4 = Write-off
  
  # Engagement
  tele.eng <- data %>%
    select(ACT_CUT_SALE, ACT_CUT_PERS, ACT_NTFP, ACT_RED_FIRE, ACT_CONTROL_BURN, ACT_INVA,
           ACT_INS, ACT_ROAD, ACT_TRAIL, ACT_WILD, ACT_GRAZE) %>% # ACT_OTH
    # mutate(across(.fns = as.character)) %>%
    mutate_all(.funs = as.character) %>%
    # mutate(across(.fns = as.numeric)) %>%
    mutate_all(.funs = as.numeric) %>%
    mutate(TELE_ACT_COUNT =rowSums(.)) %>%
    mutate(TELE_ENGAGEMENT = factor(case_when(TELE_ACT_COUNT == 0 ~ 0,
                                              TELE_ACT_COUNT == 1 ~ 1,
                                              TELE_ACT_COUNT %in% 2:3 ~ 2,
                                              TELE_ACT_COUNT >= 4 ~ 3))) %>%
    select(TELE_ACT_COUNT, TELE_ENGAGEMENT)
  
  # Combine
  tele <- bind_cols(tele.att, tele.prime, tele.eng) %>%
    select(TELE_ATT_FINANCIAL, TELE_ATT_AMENITY, TELE_ATT,
           TELE_PRIME_ATTITUDE, TELE_PRIME_ATTITUDE_CAT, TELE_PRIME_BEHAVIOR, TELE_PRIME_BEHVAIOR_CAT, TELE_PRIME,
           TELE_ACT_COUNT, TELE_ENGAGEMENT)
  
  data %>% bind_cols(tele)
}
