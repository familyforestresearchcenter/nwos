#' NWOS Region
#'
#' Calculate national, regional, and subregional summaries
#' @usage nwos_region(svl = svl, data = own, states.ref = STATES_REF)
#' @param svl = svl, data = own, states.ref = STATES_REF
#' @keywords nwos
#' @details
#' @export
#' @examples
#' ??

nwos_region <- function(svl = svl, data = own, states.ref = STATES_REF) {
  x <- bind_rows(bind_cols(svl, VALUE = data) %>%
                   left_join(states.ref %>% select(STATE, SUBREGION), by = "STATE") %>%
                   select(-STATE) %>%
                   group_by(SUBREGION, VARIABLE, LEVEL) %>%
                   summarize_all(sum) %>%
                   ungroup() %>%
                   rename(STATE = SUBREGION),
                 bind_cols(svl, VALUE = data) %>%
                   left_join(states.ref %>% select(STATE, REGION), by = "STATE") %>%
                   select(-STATE) %>%
                   group_by(REGION, VARIABLE, LEVEL) %>%
                   summarize_all(sum) %>%
                   ungroup() %>%
                   rename(STATE = REGION),
                 bind_cols(svl, VALUE = data) %>%
                   select(-STATE) %>%
                   group_by(VARIABLE, LEVEL) %>%
                   summarize_all(sum) %>%
                   ungroup() %>%
                   mutate(STATE = "N.1"))
  list(rvl = x %>% select(STATE, VARIABLE, LEVEL), data = x %>% select(-STATE, -VARIABLE, -LEVEL))
}
