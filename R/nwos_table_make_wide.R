#' nwos_table_make_wide
#'
#' Create the body of an NWOS table
#' @examples
#' x <- nwos_table_make_wide(data = readRDS("INPUTS/ESTIMATES/NWOS_2018_FFO_TENPLUS_US.RDS"))
#'
#' @export

nwos_table_make_wide <- function(data) {
  full_join(data %>%
              filter(UNITS %in% "ACRES", STATISTIC %in% "TOTAL") %>%
              rename(AC = VALUE, AC_VAR = VARIANCE) %>% select(-STATISTIC, -UNITS),
            data %>%
              filter(UNITS %in% "OWNERSHIPS", STATISTIC %in% "TOTAL") %>%
              rename(OWN = VALUE, OWN_VAR = VARIANCE) %>% select(-STATISTIC, -UNITS),
            by = c("GEO_ABB", "STRATUM", "DOMAIN", "VARIABLE", "LEVEL")) %>%
    full_join(data %>%
                filter(UNITS %in% "ACRES", STATISTIC %in% "PROPORTION") %>%
                rename(AC_PROP = VALUE, AC_PROP_VAR = VARIANCE) %>% select(-STATISTIC, -UNITS),
              by = c("GEO_ABB", "STRATUM", "DOMAIN", "VARIABLE", "LEVEL")) %>%
    full_join(data %>%
                filter(UNITS %in% "OWNERSHIPS", STATISTIC %in% "PROPORTION") %>%
                rename(OWN_PROP = VALUE, OWN_PROP_VAR = VARIANCE) %>% select(-STATISTIC, -UNITS),
              by = c("GEO_ABB", "STRATUM", "DOMAIN", "VARIABLE", "LEVEL")) %>%
    full_join(data %>%
                filter(STATISTIC %in% "N") %>%
                rename(N = VALUE) %>% select(-STATISTIC, -UNITS, -VARIANCE),
              by = c("GEO_ABB", "STRATUM", "DOMAIN", "VARIABLE", "LEVEL")) %>%
    full_join(data %>%
                filter(UNITS %in% "OWNERSHIPS", STATISTIC %in% "MEAN") %>%
                rename(MEAN = VALUE, MEAN_VAR = VARIANCE) %>% select(-STATISTIC, -UNITS),
              by = c("GEO_ABB", "STRATUM", "DOMAIN", "VARIABLE", "LEVEL")) %>%
    full_join(data %>%
                filter(UNITS %in% "OWNERSHIPS", STATISTIC %in% "MEDIAN") %>%
                rename(MEDIAN = VALUE) %>% select(-STATISTIC, -UNITS),
              by = c("GEO_ABB", "STRATUM", "DOMAIN", "VARIABLE", "LEVEL")) %>%
    full_join(data %>%
                filter(UNITS %in% "OWNERS", STATISTIC %in% "TOTAL") %>%
                rename(OWNERS = VALUE, OWNERS_VAR = VARIANCE) %>% select(-STATISTIC, -UNITS),
              by = c("GEO_ABB", "STRATUM", "DOMAIN", "VARIABLE", "LEVEL")) %>%
    mutate(LEVEL = as.character(LEVEL)) %>%
    arrange(GEO_ABB, STRATUM, DOMAIN, VARIABLE, LEVEL)
}

