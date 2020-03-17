#' nwos_estimates_summary
#'
#' Combine and ummarize Imputations
#'
#' @examples
#' domain = "FFO"
#' stratum = "ONEPLUS"
#' nwos_estimates_summary()
#'
#' @export
#'

nwos_estimates_summary <- function(geo.list = GEO_LIST, domain = "FFO", stratum = "TENPLUS") {
  geo.abb <- geo.list$GEO_ABB

  for(i in geo.abb) {
    data <- readRDS(paste0("DATA/", domain, "/", stratum, "/GEO/" ,
                           domain, "_", stratum, "_", i, ".RDS"))

    m <- data %>% filter(!is.na(IMP)) %>% distinct(IMP) %>% count() %>% pull()

    estimates.mean <- data %>%
      filter(REP == "0") %>%
      select(-IMP, -REP) %>%
      group_by(GEO, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
      summarize_all(mean) %>%
      ungroup()

    estimates.var.within <- data %>%
      filter(REP != "0") %>%
      select(-REP) %>%
      group_by(GEO, VARIABLE, LEVEL, IMP, STATISTIC, UNITS) %>%
      summarize(VAR_IMP = var(VALUE)) %>%
      ungroup() %>%
      select(-IMP) %>%
      group_by(GEO, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
      summarize(VAR_WITHIN = mean(VAR_IMP)) %>%
      ungroup()

    estimates.var.between <- data %>%
      filter(REP == "0") %>%
      select(-IMP, -REP) %>%
      group_by(GEO, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
      summarize(VAR_BETWEEN = var(VALUE)) %>%
      ungroup()

    estimates <- estimates.mean %>%
      left_join(estimates.var.within) %>%
      left_join(estimates.var.between) %>%
      mutate(VARIANCE = VAR_WITHIN + ((1 + (1 / m)) * VAR_BETWEEN)) %>%
      select(GEO, VARIABLE, LEVEL, STATISTIC, UNITS, VALUE, VARIANCE)

    saveRDS(estimates, paste0("DATA/", domain, "/", stratum, "/NWOS_2018_" ,
                              domain, "_", stratum, "_", i, ".RDS"))
  }
}
