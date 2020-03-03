#' nwos_estimates_summary
#'
#' Combine and ummarize Imputations
#'
#' @examples
#' nwos_estimates_summary()
#'
#' @export

nwos_estimates_summary <- function(directory = "DATA/FFO/ONEPLUS/") {
  files <- list.files(directory, pattern = "RDS", full.names = T)
  estimates.imp <- as_tibble(do.call(rbind, lapply(files, readRDS)))

  estimates.imp.mean <- estimates.imp %>%
    select(-IMPUTATION) %>%
    group_by(GEO_ABB, GEO_CD, DOMAIN_ABB, OWN_ABB, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
    summarize_all(mean) %>%
    ungroup() %>%
    mutate(IMPUTATION = as.numeric(NA))
  estimates.imp.var <- estimates.imp %>%
    select(-IMPUTATION, -VARIANCE_ESTIMATE) %>%
    group_by(GEO_ABB, GEO_CD, DOMAIN_ABB, OWN_ABB, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
    summarize_all(var) %>%
    ungroup() %>%
    mutate(IMPUTATION = as.numeric(NA)) %>%
    rename(VARIANCE_IMPUTATION = VALUE)

  bind_rows(full_join(estimates.imp.mean, estimates.imp.var,
                      by = c("GEO_ABB", "GEO_CD", "DOMAIN_ABB",
                             "OWN_ABB", "VARIABLE", "LEVEL", "STATISTIC",
                             "UNITS", "IMPUTATION")) %>%
              mutate(VARIANCE = VARIANCE_ESTIMATE + VARIANCE_IMPUTATION),
            estimates.imp) %>%
    arrange(GEO_CD, !is.na(IMPUTATION), IMPUTATION, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
    select(GEO_ABB, GEO_CD, DOMAIN_ABB, OWN_ABB, IMPUTATION,
           VARIABLE, LEVEL, STATISTIC, UNITS,
           VALUE, VARIANCE, VARIANCE_ESTIMATE, VARIANCE_IMPUTATION)
}

