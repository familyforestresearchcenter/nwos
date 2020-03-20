#' nwos_table_make_coop
#' @examples
#' # data <- readRDS("INPUTS/ESTIMATES/NWOS_2018_FFO_COOP.RDS")
#' nwos_table_make_coop(data)
#' @export

nwos_table_make_coop <- function(COOP) {
  COOP <- COOP %>%
    filter(GEO_CD %in% (GEO_LIST %>% filter(GEO_LEVEL %in% c("STATE", "SUBSTATE")) %>% pull(GEO_CD))) %>%
    left_join(GEO_LIST %>% select(GEO_ABB, GEO_CD))

  GEO_LIST_REGION <- GEO_LIST %>% filter(GEO_LEVEL %in% c("NATION", "REGION", "SUBREGION") | GEO_ABB %in% c("OK", "TX"))

  COOP_REGION <- bind_rows(lapply(1:NROW(GEO_LIST_REGION),
                                  function(x) {
                                    COOP %>%
                                      filter(GEO_CD %in% unlist(strsplit(GEO_LIST_REGION$GEO_CD[x], split = ", "))) %>%
                                      select(RESPONSE_CAT, COUNT) %>%
                                      group_by(RESPONSE_CAT) %>%
                                      summarize(COUNT = sum (COUNT)) %>%
                                      ungroup() %>%
                                      mutate(GEO_CD = GEO_LIST_REGION$GEO_CD[x],
                                             GEO_ABB = GEO_LIST_REGION$GEO_ABB[x])}))
  COOP %>% bind_rows(COOP_REGION)
}
