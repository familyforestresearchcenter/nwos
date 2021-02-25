#' nwos_table_make_wide
#'
#' Convert long NWOS estimates to wide and add descriptors
#' @examples
#' REF_GEO <- read_csv("DATA/REF/REF_GEO.csv")
#' REF_VARIABLE <- read_csv("DATA/REF/REF_VARIABLE.csv")
#' REF_TABLE <- read_csv("DATA/REF/REF_TABLE.csv")
#' REF_LABEL <- read_csv("DATA/REF/REF_LABEL.csv", col_types = list(col_character(), col_character(), col_character(), col_double()))
# data <- readRDS("DATA/NWOS_2018_FFO_CORE_CARBON.RDS")
#' x <- nwos_table_make_wide(data)
#'
#' @export

nwos_table_make_wide <- function(data, ref.geo = REF_GEO, ref.variable = REF_VARIABLE,
                                 ref.table = REF_TABLE, ref.label = REF_LABEL) {
  require(tidyverse)

  data %>%
  filter(!STATISTIC %in% c("MEAN", "MEDIAN"), !UNITS %in% c("OWNERS")) %>%
    pivot_wider(names_from = c(UNITS, STATISTIC),
                # names_prefix = NA,
                names_glue = "{.name}_{.value}",
                values_from = c(VALUE, VARIANCE)) %>%
    left_join(ref.geo %>% select(GEO_ABB, GEO_NAME), by = "GEO_ABB") %>%
    select(-GEO_ABB) %>%
    mutate(STRATUM = if_else(STRATUM == "FFO", "Family", STRATUM)) %>%
    left_join(ref.variable, by = "VARIABLE") %>%
    left_join(ref.label, by = c("VARIABLE", "LEVEL")) %>%
    left_join(ref.table, by = c("TABLE", "SUBTABLE")) %>%
    select(GEO_NAME, TABLE, SUBTABLE, LABEL, VARIABLE, everything())
}

