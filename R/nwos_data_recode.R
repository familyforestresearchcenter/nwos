#' NWOS Data Recode
#'
#' Recode and add basic NWOS variables.
#' @usage nwos_data_recode(data = QUEST_WIDE, meta.data = QUEST_META, box2 = c(names(data)[grepl("OBJ_", names(data))], names(data)[grepl("CNC_", names(data))], "ATT_WOODED", "ATT_SELL", "KNOW_WOOD", "WANT_KNOW_WOOD", "EMO_WOOD", "WOOD_COMMUNITY"))
#' @param data = QUEST_WIDE,
#' @param meta.data = QUEST_META,
#' @param box2 = c(names(data)[grepl("OBJ_", names(data))], names(data)[grepl("CNC_", names(data))], "ATT_WOODED", "ATT_SELL", "KNOW_WOOD", "WANT_KNOW_WOOD", "EMO_WOOD", "WOOD_COMMUNITY")
#' @keywords nwos
#' @details
#' The default values create the variables used in the NWOS tables.
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' ??

nwos_data_recode <- function(data = QUEST_WIDE,
                             meta.data = QUEST_META,
                             box2 = c(names(data)[grepl("OBJ_", names(data))], names(data)[grepl("CNC_", names(data))],
                                      "ATT_WOODED", "ATT_SELL", "KNOW_WOOD", "WANT_KNOW_WOOD", "EMO_WOOD", "WOOD_COMMUNITY")) {
  for(i in 1:length(box2)) data[[paste0(box2[i], "_BOX2")]] <- data[[box2[i]]] # Copy data to new BOX2 variables
  data %>%
    # Recode data types
    mutate_at(vars(meta.data %>% filter(DATA_TYPE %in% "INTEGER") %>% pull(COLUMN)),
              funs(as.integer))%>%
    mutate_at(vars(meta.data %>% filter(DATA_TYPE %in% "FACTOR") %>% pull(COLUMN)),
              funs(factor)) %>%
    mutate_at(vars(meta.data %>% filter(DATA_TYPE %in% c("LIKERT", "ORDERED")) %>% pull(COLUMN)),
              funs(ordered)) %>%
    mutate(
      # Convert numeric variables to categorical
      AC_LAND_CAT = cut(AC_LAND, c(1, 10, 20, 50, 100, 200, 500, 1000, 5000, Inf), c(1, 10, 20, 50, 100, 200, 500, 1000, 5000), include.lowest = T, right = F, ordered_result = T),
      AC_WOOD_CAT = cut(AC_WOOD, c(1, 10, 20, 50, 100, 200, 500, 1000, 5000, Inf), c(1, 10, 20, 50, 100, 200, 500, 1000, 5000), include.lowest = T, right = F, ordered_result = T),
      OWNERS_NUMBER_CAT = cut(OWNERS_NUMBER, c(1, 2, 3, 6, 10, Inf), c(1, 2, 3, 6, 10), include.lowest = T, right = F, ordered_result = T),
      TENURE_CAT = cut(NWOSYR - ACQ_YEAR, c(0, 10, 25, 50, Inf), c(0, 10, 25, 50), include.lowest = T, right = F, ordered_result = T),
      OWN1_AGE_CAT = cut(OWN1_AGE, c(18, 45, 55, 65, 75, Inf), c(18, 45, 55, 65, 75), include.lowest = T, right = F, ordered_result = T),
      INC_WOOD_CAT = cut(INC_WOOD, c(0, 1, 5, 20, 50, Inf), c(0, 1, 5, 20, 50), include.lowest = T, right = F, ordered_result = T),
      # Add total variable
      TOTAL = factor(1, levels = c(0, 1)),
      # Change 8 to 0
      HOME_2 = factor(if_else(HOME %in% 8, "0", as.character(HOME))),
      CABIN_2 = factor(if_else(CABIN %in% 8, "0", as.character(CABIN))),
      FARM_2 = factor(if_else(FARM %in% 8, "0", as.character(FARM)))) %>%
    # Box2s
    mutate_at(vars(contains("_BOX2")), ~factor(if_else(. %in% c("5", "4"), "1", "0"))) %>%
    # Drop unused factor levels
    mutate_if(is.factor, factor)
}
