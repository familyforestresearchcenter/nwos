#' nwos_tables_data
#'
#' Create the body of an NWOS core, area, or cooperataion rate table
#' @param data table.data
#' @details For area and cooperation rate tables see ...
#' data <- nwos_table_make_wide(readRDS("INPUTS/ESTIMATES/NWOS_2018_FFO_TENPLUS_US.RDS"))
#' units = "thousands"
#' ref.geo = REF_GEO
#' ref.tab = REF_TABLE
#' ref.var = REF_VARIABLE
#' ref.lab = REF_LABEL
#' nwos_table_data()
#' @export

nwos_table_data <- function(data, units = "thousands",
                            ref.geo = REF_GEO, ref.tab = REF_TABLE,
                            ref.var = REF_VARIABLE, ref.lab = REF_LABEL) {

  units.adj <- ifelse(units %in% "thousands", 1e3, 1)
  categorical <- data %>%
    filter(!VARIABLE %in% "TOTAL") %>%
    mutate(AC = AC / units.adj,
           AC_SE = sqrt(AC_VAR) / units.adj,
           OWN = OWN / units.adj,
           OWN_SE = sqrt(OWN_VAR) / units.adj,
           AC_PERC = AC_PROP * 100,
           AC_PERC_SE = sqrt(AC_PROP_VAR) * 100,
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
           AC, AC_SE, OWN , OWN_SE, AC_PERC, AC_PERC_SE, OWN_PERC,OWN_PERC_SE, N,
           everything()) %>%
    filter(!is.na(TABLE_NUMBER), !is.na(LABEL), SUBTABLE != "FOOTNOTE") %>%
    arrange(GEO_ABB, TABLE_NUMBER, SUBTABLE_NUMBER, ORDER)

  continuous <- data %>%
    filter(!is.na(MEAN)) %>%
    mutate(MEAN_SE = sqrt(MEAN_VAR),
           OWNERS_SE = sqrt(OWNERS_VAR)) %>%
    left_join(ref.var, by = "VARIABLE") %>%
    left_join(ref.lab, by = c("VARIABLE", "LEVEL")) %>%
    left_join(ref.tab, by = c("TABLE", "SUBTABLE")) %>%
    select(GEO_ABB,
           TABLE_NUMBER, TABLE,
           VARIABLE, LABEL,
           MEAN, MEAN_SE, MEDIAN, OWNERS, OWNERS_SE) %>%
    filter(!is.na(TABLE_NUMBER), !is.na(LABEL)) %>%
    arrange(GEO_ABB, TABLE_NUMBER)

  list(cat = categorical, cont = continuous)
}
