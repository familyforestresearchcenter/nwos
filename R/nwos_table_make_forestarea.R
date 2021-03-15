#' nwos_table_make_forestarea
#' @examples
#' AREA_AL <- readRDS("INPUTS/ESTIMATES/NWOS_FOREST_AREA_2018_20190909.RDS") %>% filter(STATECD == 1)
#' nwos_table_make_forestarea(readRDS("INPUTS/ESTIMATES/NWOS_FOREST_AREA_2018_20190909.RDS"))
#' @export

nwos_table_make_forestarea <- function(AREA) {
  state.substate.list <-  unlist(strsplit(REF_GEO %>% filter(GEO_LEVEL %in% c("STATE", "SUBSTATE")) %>% pull(GEO_CD), split = ", "))
  area <- AREA %>%
    select(GEO_CD = STATECD, OWNGRP, ACRES, ACRES_VARIANCE) %>%
    mutate(GEO_CD = as.character(GEO_CD)) %>%
    filter(GEO_CD %in% state.substate.list) %>%
    left_join(REF_GEO %>% select(GEO_ABB, GEO_CD), by = "GEO_CD") %>%
    group_by(GEO_CD, GEO_ABB, OWNGRP) %>%
    summarize(ACRES = sum(ACRES),
              ACRES_VARIANCE = sum(ACRES_VARIANCE)) %>%
    ungroup()


  geo.region.list <- REF_GEO %>% filter(GEO_LEVEL %in% c("NATION", "REGION", "SUBREGION") | GEO_ABB %in% c("OK", "TX"))

  area.region <- bind_rows(lapply(1:NROW(geo.region.list),
                                  function(x) {
                                    area %>%
                                      filter(GEO_CD %in% unlist(strsplit(geo.region.list$GEO_CD[x], split = ", "))) %>%
                                      select(OWNGRP, ACRES, ACRES_VARIANCE) %>%
                                      group_by(OWNGRP) %>%
                                      summarize(ACRES = sum(ACRES),
                                                ACRES_VARIANCE = sum(ACRES_VARIANCE)) %>%
                                      ungroup() %>%
                                      mutate(GEO_CD = geo.region.list$GEO_CD[x],
                                             GEO_ABB = geo.region.list$GEO_ABB[x])}))
  area %>% bind_rows(area.region)
}

