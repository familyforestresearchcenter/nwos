#' nwos_tables_data
#'
#' Create the body of an NWOS core, area, or cooperataion rate table
#' @param data table.data
#' @details For area and cooperation rate tables see ...
#' data = ESTIMATES
#' ref.geo = REF_GEO
#' ref.tab = REF_TABLE
#' ref.var = REF_VARIABLE
#' ref.lab = REF_LABEL
#' nwos_table_data()
#' @export

nwos_urban_table_data <- function(data, units = NA,
                                  ref.geo = REF_GEO, ref.tab = REF_TABLE,
                                  ref.var = REF_VARIABLE, ref.lab = REF_LABEL) {

  units.adj <- ifelse(units %in% "thousands", 1e3, 1)
  data %>%
    filter(!VARIABLE %in% "TOTAL") %>%
    mutate(OWN = OWN / units.adj,
           OWN_SE = sqrt(OWN_VAR) / units.adj,
           OWN_PERC = OWN_PROP * 100,
           OWN_PERC_SE = sqrt(OWN_PROP_VAR) * 100,
           N = N) %>%
    left_join(ref.geo %>% select(GEO_ABB, GEO_NAME), by = "GEO_ABB") %>%
    left_join(ref.var, by = "VARIABLE") %>%
    left_join(ref.lab, by = c("VARIABLE", "LEVEL")) %>%
    left_join(ref.tab, by = c("TABLE", "SUBTABLE")) %>%
    select(GEO_ABB, GEO_NAME,
           TABLE_NUMBER, TABLE, SUBTABLE_NUMBER, SUBTABLE,
           VARIABLE, LEVEL, LABEL, ORDER,
           OWN, OWN_SE, OWN_PERC,OWN_PERC_SE, N,
           HEADER, DESCRIPTION, FOOTNOTE) %>%
    # filter(!is.na(TABLE_NUMBER), !is.na(LABEL), SUBTABLE != "FOOTNOTE") %>%
    arrange(GEO_ABB, TABLE_NUMBER, SUBTABLE_NUMBER, ORDER)
}
