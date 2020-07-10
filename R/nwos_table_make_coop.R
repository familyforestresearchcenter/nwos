#' nwos_table_make_coop
#' @examples
#' # data <- readRDS("INPUTS/ESTIMATES/NWOS_2018_FFO_COOP.RDS")
#' nwos_table_make_coop(data)
#' @export

nwos_table_make_coop <- function(COOP) {
  state.substate.list <-  unlist(strsplit(REF_GEO %>% filter(GEO_LEVEL %in% c("STATE", "SUBSTATE")) %>% pull(GEO_CD), split = ", "))
  coop <- COOP %>%
    filter(GEO_CD %in% state.substate.list) %>%
    left_join(REF_GEO %>% select(GEO_ABB, GEO_CD), by = "GEO_CD")

  geo.region.list <- REF_GEO %>% filter(GEO_LEVEL %in% c("NATION", "REGION", "SUBREGION") | GEO_ABB %in% c("OK", "TX"))

  coop.region <- bind_rows(lapply(1:NROW(geo.region.list),
                                  function(x) {
                                    coop %>%
                                      filter(GEO_CD %in% unlist(strsplit(geo.region.list$GEO_CD[x], split = ", "))) %>%
                                      select(RESPONSE_CAT, COUNT) %>%
                                      group_by(RESPONSE_CAT) %>%
                                      summarize(COUNT = sum (COUNT)) %>%
                                      ungroup() %>%
                                      mutate(GEO_CD = geo.region.list$GEO_CD[x],
                                             GEO_ABB = geo.region.list$GEO_ABB[x])}))
  coop %>% bind_rows(coop.region)
}
