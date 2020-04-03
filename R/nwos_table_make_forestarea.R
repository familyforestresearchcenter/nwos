REF_GEO  <- as_tibble(read.csv("INPUTS/REF/REF_GEO.csv", stringsAsFactors = F))
REF_STATE <- as_tibble(read.csv("~/Dropbox (FFRC)/NWOS/DB/REF_TABLES/REF_STATE.csv", stringsAsFactors = F)) %>%
  filter(!STATECD_NWOS %in% "2.2") %>%
  select(STATECD_NWOS, REGION_ALPHA_NWOS, SUBREGION_ALPHA) %>%
  mutate(REGION = toupper(REGION_ALPHA_NWOS),
         SUBREGION = recode(SUBREGION_ALPHA,
                            "Northeast" = "NORTHEAST",
                            "North Central" = "NORTH_CENTRAL",
                            "Southeast" = "SOUTHEAST",
                            "South Central" = "SOUTH_CENTRAL",
                            "Intermountain" = "ROCKY_MOUNTAIN",
                            "Great Plains" = "ROCKY_MOUNTAIN",
                            "Alaska" = "PACIFIC_COAST",
                            "Pacific Southwest" = "PACIFIC_COAST",
                            "Pacific Northwest" = "PACIFIC_COAST")) %>%
  select(-REGION_ALPHA_NWOS, -SUBREGION_ALPHA)

FA_STATE <- readRDS("INPUTS/ESTIMATES/NWOS_FORESTAREA_STATE_2018_20200227.RDS") %>%
  select(GEO_CD = STATECD_NWOS, OWNCAT, ACRES, VARIANCE) %>%
  left_join(REF_STATE, by = c("GEO_CD" = "STATECD_NWOS")) %>%
  distinct() %>%
  filter(!GEO_CD == 2.2) %>%
  group_by(GEO_CD, OWNCAT, REGION, SUBREGION) %>%
  summarise_all(sum) %>%
  ungroup()

FA_STATE %>% distinct(GEO_CD) %>% pull()

FA_GEO <- bind_rows(FA_STATE %>%
                      mutate(GEO_CD = as.character(GEO_CD)) %>%
                      left_join(REF_GEO %>% select(-GEO_NAME), by = "GEO_CD"),
                    FA_STATE %>%
                      filter(GEO_CD %in% c("40.1", "40.2")) %>%
                      select(OWNCAT, ACRES, VARIANCE) %>%
                      group_by(OWNCAT) %>%
                      summarize_all(sum) %>%
                      ungroup() %>%
                      mutate(GEO_ABB = "OK"),
                    FA_STATE %>%
                      filter(GEO_CD %in% c("48.1", "48.2")) %>%
                      select(OWNCAT, ACRES, VARIANCE) %>%
                      group_by(OWNCAT) %>%
                      summarize_all(sum) %>%
                      ungroup() %>%
                      mutate(GEO_ABB = "TX"),
                    FA_STATE %>%
                      select(OWNCAT, ACRES, VARIANCE) %>%
                      group_by(OWNCAT) %>%
                      summarize_all(sum) %>%
                      ungroup() %>%
                      mutate(GEO_ABB = "US"),
                    FA_STATE %>%
                      select(REGION, OWNCAT, ACRES, VARIANCE) %>%
                      group_by(REGION, OWNCAT) %>%
                      summarize_all(sum) %>%
                      ungroup() %>%
                      rename(GEO_ABB = REGION),
                    FA_STATE %>%
                      select(SUBREGION, OWNCAT, ACRES, VARIANCE) %>%
                      group_by(SUBREGION, OWNCAT) %>%
                      summarize_all(sum) %>%
                      ungroup() %>%
                      rename(GEO_ABB = SUBREGION))

FA_GEO %>% distinct(GEO_ABB) %>% pull()

FA_GEO %>% filter(GEO_ABB == "US")

saveRDS(FA_GEO, "INPUTS/ESTIMATES/NWOS_2018_FORESTAREA.RDS")

