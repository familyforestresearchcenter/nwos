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

nwos_estimates_summary <- function(geo.list = GEO_LIST, stratum = "FFO", domain = "TENPLUS") {
  geo.abb <- geo.list$GEO_ABB

  for(i in geo.abb) {
    data <- readRDS(paste0("DATA/", stratum, "/", domain, "/GEO/" ,
                            stratum, "_", domain, "_", i, ".RDS"))

    supp.data <- do.call(rbind, lapply(list.files("DATA/SUPPLEMENT/", full.names = T), readRDS)) %>%
      mutate(REP = as.numeric(REP))

    data <- data %>%
      filter(!(VARIABLE == "TRAN_RECENT" & LEVEL %in% c(-2, 0)),
                    !(VARIABLE == "CUT_FORESTER" & LEVEL %in% c(0, 8)),
                    !(VARIABLE == "EASE_5YR" & LEVEL %in% c(-2, 8))) %>%
      mutate(VARIABLE = recode(VARIABLE,
                               "CUT_FORESTER" = "CUT_FORESTER_2",
                               "EASE_5YR" =  "EASE_5YR_2")) %>%
      bind_rows(supp.data)


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
      mutate(VARIANCE = VAR_WITHIN + ((1 + (1 / m)) * VAR_BETWEEN),
             STRATUM = stratum,
             DOMAIN = domain) %>%
      select(GEO_ABB = GEO, STRATUM, DOMAIN, VARIABLE, LEVEL, STATISTIC, UNITS, VALUE, VARIANCE)

    estimates %>% filter(VARIABLE == "OWNERS_NUMBER")
    estimates %>% filter(VARIABLE == "CUT_FORESTER_2")
    estimates %>% filter(VARIABLE == "EASE_5YR_2") %>% distinct(LEVEL)

    saveRDS(estimates, paste0("DATA/", stratum, "/", domain, "/NWOS_2018_" ,
                              stratum, "_", domain, "_", i, ".RDS"))
  }
}
