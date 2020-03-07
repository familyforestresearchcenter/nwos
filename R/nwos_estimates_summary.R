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

# # Summarize by rep
# estimates.imp <- left_join(estimates.imp.rep %>%
#                              group_by(IMP) %>%
#                              summarize_all(mean) %>%
#                              ungroup(),
#                            estimates.imp.var <- estimates.imp.rep %>%
#                              group_by(IMP) %>%
#                              summarize_all(var) %>%
#                              ungroup() %>%
#                              rename_at(vars(2:5), funs(paste0(. ,"_VAR"))),
#                            by = "IMP")
# # Summarize by imp
# estimates.imp.var <- estimates.imp %>%
#   select(-IMP, -ends_with("_VAR")) %>%
#   summarize_all(var) %>%
#   rename_all(funs(paste0(. ,"_IMP_VAR")))
# estimates <- estimates.imp %>%
#   select(-IMP) %>%
#   summarize_all(mean) %>%
#   mutate(OWN_VAR = OWN_VAR + estimates.imp.var$OWN_IMP_VAR,
#          AC_VAR = AC_VAR + estimates.imp.var$AC_IMP_VAR,
#          OWN_PROP_VAR =  OWN_PROP_VAR + estimates.imp.var$OWN_PROP_IMP_VAR,
#          AC_PROP_VAR = AC_PROP_VAR + estimates.imp.var$AC_PROP_IMP_VAR,
#          VARIABLE = variable, LEVEL = level) %>%
#   select(VARIABLE, LEVEL, everything())

# own <- nwos_estimates_total_geo(geo = geo, domain = domain)
# own.rep <- nwos_estimates_total_geo(geo = geo, domain = domain, reps = reps)
# ac.oneplus <- nwos_estimates_total_geo(geo = GEO[1,], domain = "ONEPLUS",
#                                        area = "AC_WOOD")
# ac.oneplus.rep <- nwos_estimates_total_geo(geo = GEO[1,],
#                                            domain = "ONEPLUS",
#                                            area = "AC_WOOD",
#                                            reps = 1:10)
# n.oneplus <- nwos_estimates_n_geo(geo = GEO[1,], domain = "ONEPLUS")
# mean.oneplus <- nwos_estimates_mean_geo(geo = GEO[1,], domain = "ONEPLUS")
# mean.oneplus.rep <- nwos_estimates_mean_geo(geo = GEO[1,], domain = "ONEPLUS", reps = 1:10)
# median.oneplus <- nwos_estimates_median_geo(geo = GEO[1,], domain = "ONEPLUS")
# median.oneplus.rep <- nwos_estimates_median_geo(geo = GEO[1,], domain = "ONEPLUS", reps = 1:10)
#
# estimates.imp.rep <- bind_rows(own.oneplus, own.oneplus.rep,
#                                ac.oneplus, ac.oneplus.rep,
#                                n.oneplus,
#                                mean.oneplus, mean.oneplus.rep,
#                                median.oneplus, median.oneplus.rep)
# # estimates.imp.rep
#
# # Proportions
# total.imp.rep <- estimates.imp.rep %>% filter(VARIABLE %in% "TOTAL", STATISTIC %in% "TOTAL") %>%
#   select(-VARIABLE, -LEVEL) %>% rename(TOTAL = VALUE)
# proportion.imp.rep <- estimates.imp.rep %>% filter(STATISTIC %in% "TOTAL") %>%
#   left_join(total.imp.rep,
#             by = c("GEO_ABB", "STRATUM", "DOMAIN", "STATISTIC", "UNITS", "IMPUTATION", "REP")) %>%
#   mutate(PROPORTION = VALUE / TOTAL, STATISTIC = "PROPORTION") %>%
#   select(-VALUE, - TOTAL) %>% rename(VALUE = PROPORTION)
# estimates.prop.imp.rep <-  bind_rows(estimates.imp.rep, proportion.imp.rep)
# estimates.prop.imp.rep
#
# # Variances
# variance.prop.imp.rep <- estimates.prop.imp.rep %>%
#   filter(STATISTIC %in% c("TOTAL", "PROPORTION", "MEAN", "MEDIAN"), !is.na(REP)) %>%
#   group_by(GEO_ABB, STRATUM, DOMAIN, VARIABLE, LEVEL, STATISTIC, UNITS, IMPUTATION) %>%
#   summarize(VARIANCE = var(VALUE)) %>%
#   ungroup()
#
# estimates.prop.imp.var <- estimates.prop.imp.rep %>% filter(is.na(REP)) %>% select(-REP) %>%
#   left_join(variance.prop.imp.rep,
#             by = c("GEO_ABB",  "STRATUM", "DOMAIN", "VARIABLE", "LEVEL", "STATISTIC", "UNITS", "IMPUTATION"))
#
# estimates.prop.imp.var %>%
#   select(-IMPUTATION) %>%
#   group_by(GEO_ABB, STRATUM, DOMAIN, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
#   summarize_all(mean)
# ungroup()


