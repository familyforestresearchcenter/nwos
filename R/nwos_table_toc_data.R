#' nwos_table_toc_data

nwos_table_toc_data <- function(geo.abb, ref.geo = REF_GEO, ref.tab = REF_TABLE,
                                stratum = stratum.name, domain = domain.name,
                                yr = year, yr.range = year.range) {
  ref.tab %>% select(TABLE_NUMBER, TABLE, DESCRIPTION) %>%
    filter(!DESCRIPTION %in% "CONTINUED") %>%
    distinct() %>%
    mutate(TABLE_NUMBER = as.integer(TABLE_NUMBER),
           STRATUM_NAME = stratum,
           DOMAIN_NAME = domain,
           GEO_ABB = geo.abb,
           GEO_NAME = ref.geo %>% filter(GEO_ABB %in% geo.abb) %>% pull(GEO_NAME),
           YEAR = yr,
           YEAR_RANGE = yr.range) %>%
    arrange(TABLE_NUMBER)
}



