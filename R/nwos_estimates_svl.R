#' NWOS States, Variables, and Levels
#'
#' Generate List of States, Variables, and Levels used for NWOS functions
#' @usage nwos_svl(data = QUEST_WIDE)
#' @param data = QUEST_WIDE
#' @keywords nwos
#' @details
#' @export
#' @examples
#' ??

nwos_svl <- function(data = QUEST_WIDE) {
  states <- unique(data$STATECD_NWOS)
  vl <- lapply(names(data), function(x) levels(data %>% pull(get(x))))
  names(vl) <- names(data)
  vl <- tibble(VARIABLE = if_else(names(unlist(vl)) %in% "TOTAL", "TOTAL",
                                  substr(names(unlist(vl)), 1, nchar(names(unlist(vl))) - 1)),
               LEVEL = unlist(vl))
  tibble(STATE = rep(states, each = NROW(vl)),
         VARIABLE = rep(vl$VARIABLE, NROW(states)),
         LEVEL = rep(vl$LEVEL, NROW(states))) %>%
    arrange(as.numeric(STATE))
}
