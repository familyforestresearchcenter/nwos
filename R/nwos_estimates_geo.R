#' nwos_estimates_geo
#'
#' Generate estimates by geography
#'
#' @param domain = "AC_WOOD >= 10",
#' @param geo = GEO,
#' @param cond.status = 1,
#' @param own = 45,
#' @param quest = QUEST,
#' @param plot = PLOTS,
#' @param imputations = 1:5
#'
#' @return data frame
#'
#' @examples
#' nwos_estimates_geo(imputations = 1:2)
#' nwos_estimates_geo(imputations = 1:2, domain = "AC_WOOD >= 1")
#' nwos_estimates_geo(domain = "AC_WOOD >= 1")
#'
#' @export

nwos_estimates_geo <- function(domain = "AC_WOOD >= 10",
                               geo = GEO,
                               cond.status = 1,
                               own = 45,
                               quest = QUEST,
                               quest.meta = QUEST_META,
                               imputations = 1:5,
                               variables.categorcial = VARIABLES_CATEGORICAL,
                               variables.continuous = VARIABLES_CONTINUOUS) {
  # Generate estimates by GEO and IMPUTATION
  estimates <- do.call(rbind, mcmapply(nwos_estimates_geo_imp,
                                       geo.cd.list = geo$GEO_CD,
                                       imp = imputations,
                                       domain = domain,
                                       # quest = quest,
                                       SIMPLIFY = F,
                                       mc.cores = detectCores() - 1)) %>%
    rename(VARIANCE_ESTIMATE = VARIANCE)

  # Summarize Imputations
  estimates.imp.mean <- estimates %>%
    select(-IMPUTATION) %>%
    group_by(GEO_CD, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
    summarize_all(mean) %>%
    ungroup() %>%
    mutate(IMPUTATION = as.numeric(NA))
  estimates.imp.var <- estimates %>%
    select(-IMPUTATION, -VARIANCE_ESTIMATE) %>%
    group_by(GEO_CD, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
    summarize_all(var) %>%
    ungroup() %>%
    mutate(IMPUTATION = as.numeric(NA)) %>%
    rename(VARIANCE_IMPUTATION = VALUE)
  estimates.imp <- full_join(estimates.imp.mean, estimates.imp.var,
                             by = c("GEO_CD", "VARIABLE", "LEVEL", "STATISTIC", "UNITS", "IMPUTATION")) %>%
    mutate(VARIANCE = VARIANCE_ESTIMATE + VARIANCE_IMPUTATION)

  bind_rows(estimates, estimates.imp) %>%
    arrange(GEO_CD, !is.na(IMPUTATION), IMPUTATION, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
    mutate(COND_STATUS_CD = cond.status,
           OWNCD_NWOS = own,
           DOMAIN = domain) %>%
    select(GEO_CD, COND_STATUS_CD, OWNCD_NWOS, DOMAIN, IMPUTATION,
           VARIABLE, LEVEL, STATISTIC, UNITS,
           VALUE, VARIANCE, VARIANCE_ESTIMATE, VARIANCE_IMPUTATION)
}

