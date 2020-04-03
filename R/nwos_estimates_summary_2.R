#' nwos_estimates_summary
#'
#' Combine and ummarize Imputations
#'
#' @examples
#' domain = "FFO"
#' stratum = "ONEPLUS"
#' # Need to run nwos_estimates_combine first
#' nwos_estimates_summary(geo.list = GEO_LIST[55,])
#'
#' @export
#'

nwos_estimates_summary_2 <- function(data) {

    m <- data %>% filter(!is.na(IMP)) %>% distinct(IMP) %>% count() %>% pull()

    # Add proportions
    total <- data %>% filter(VARIABLE == "TOTAL", STATISTIC == "TOTAL") %>% rename("TOTAL" = "VALUE") %>% select(-VARIABLE, -LEVEL)
    prop <- data %>% filter(STATISTIC == "TOTAL") %>% left_join(total) %>% mutate(PROPORTION = VALUE / TOTAL) %>%
      select(-VALUE, -TOTAL) %>% rename(VALUE = PROPORTION) %>% mutate(STATISTIC = "PROPORTION")
    data <- data %>% bind_rows(prop)

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
      select(GEO_ABB = GEO, VARIABLE, LEVEL, STATISTIC, UNITS, VALUE, VARIANCE)

    return(estimates)
}
