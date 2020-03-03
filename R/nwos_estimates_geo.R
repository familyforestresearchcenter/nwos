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
#' nwos_estimates_geo(geo = tibble(GEO_CD = c("1", "5"), GEO_ABB = c("AL", "AR")),
#' variable.categorical = tibble(VARIABLE = c("OWNTYPE", "OWNTYPE"), LEVEL = c("1", "2")),
#' variable.continuous = NA)
#' domain = tibble(DOMAIN_STRING = "AC_WOOD >= 10", DOMAIN_ABB = "TENPLUS"),
#' geo = tibble(GEO_CD = "1", GEO_ABB = "AL"
#' nwos_estimates_geo( dom = "TEST", imputation = 1, variable.categorical = VARIABLES_CATEGORICAL[1,], VARIABLES_CONTINUOUS[1])
#' nwos_estimates_geo(geo.cd = "1", imputation = 2, variable.categorical = VARIABLES_CATEGORICAL[1,])
#' nwos_estimates_geo(geo.cd = "1", imputation = 1:2, variable.categorical = VARIABLES_CATEGORICAL[1,])
#' 
#' nwos_estimates_geo(geo.cd = "1", imputation = 1, variable.continuous = VARIABLES_CONTINUOUS[1])
#' nwos_estimates_geo(geo.cd = "1", imputation = 1:2, variable.continuous = VARIABLES_CONTINUOUS[1])
#' 
#' nwos_estimates_geo(geo.cd = "1", imputation = 1, variable.categorical = VARIABLES_CATEGORICAL[1,], variable.continuous = VARIABLES_CONTINUOUS[1])
#' nwos_estimates_geo(geo.cd = "1", imputation = 1:2, variable.categorical = VARIABLES_CATEGORICAL[1,], variable.continuous = VARIABLES_CONTINUOUS[1])
#' nwos_estimates_geo(geo.cd = c("1", "5"), imputation = 1, variable.categorical = VARIABLES_CATEGORICAL[1,], variable.continuous = VARIABLES_CONTINUOUS[1])
#' nwos_estimates_geo(geo.cd = c("1", "5"), imputation = 1:2, variable.categorical = VARIABLES_CATEGORICAL[1,], variable.continuous = VARIABLES_CONTINUOUS[1])
#'
#' @export

nwos_estimates_geo <- function(domain,
                               geo,
                               cond.status.cd = 1,
                               own = tibble(OWN_CD = 45, OWN_ABB = "FFO"),
                               quest = QUEST,
                               quest.meta = QUEST_META,
                               imputation = 1:5,
                               variable.categorical = VARIABLES_CATEGORICAL,
                               variable.continuous = VARIABLES_CONTINUOUS,
                               var.method = "approx",
                               out.path = "DATA/FFO/") {
  options(warn=-1)
  for(i in 1:NROW(geo))
  {
    geo.cd <- geo$GEO_CD[i]
    geo.abb <- geo$GEO_ABB[i]
    print(paste("GEO:", geo.abb))
    estimates <- tibble()
    for(j in imputation)
    {
      print(paste("Imputation:", j)) # j = 2
      quest.imp <- quest[[j]] %>%
        filter(STATECD_NWOS %in% unlist(strsplit(geo$GEO_CD, split=", ")))
      
      design.own <- twophase(id = list(~ 1, ~ CN),
                             strata = list(~ STATECD_NWOS + COND_STATUS_CD + OWNCD_NWOS, NULL),
                             subset = ~ RESP,
                             probs = list(NULL, ~ PROB),
                             data = quest.imp,
                             method = var.method)
      design.ac <-  twophase(id = list(~ 1, ~ CN),
                             strata = list(~ STATECD_NWOS + COND_STATUS_CD + OWNCD_NWOS, NULL),
                             subset = ~ RESP,
                             probs = list(NULL, ~ PROB_AC),
                             data = quest.imp,
                             method = var.method)
      
      if(!is.na(variable.categorical)) {
        estimates <- estimates %>% 
          bind_rows(do.call(rbind, mcmapply(nwos_estimates_categorical,
                                  domain = domain$DOMAIN_STRING,
                                  variable = variable.categorical$VARIABLE,
                                  level = variable.categorical$LEVEL,
                                  MoreArgs = list(design.own = design.own,
                                                  design.ac = design.ac,
                                                  quest = quest.imp),
                                  SIMPLIFY = F,
                                  mc.cores = detectCores() - 1)) %>%
          rename(VARIANCE_ESTIMATE = VARIANCE )%>% 
            mutate(IMPUTATION = j))
      }
      if(!is.na(variable.continuous)) {
        estimates <- estimates %>%
          bind_rows(do.call(rbind, mcmapply(nwos_estimates_continuous,
                                  domain = domain$DOMAIN_STRING,
                                  variable = variable.continuous,
                                  MoreArgs = list(design.own = design.own),
                                  SIMPLIFY = F,
                                  mc.cores = detectCores() - 1)) %>%
          rename(VARIANCE_ESTIMATE = VARIANCE) %>% 
            mutate(IMPUTATION = j))
      }
      rm(quest.imp, design.own, design.ac)
      gc()
    }
    estimates <- estimates %>% mutate(GEO_CD = geo.cd) %>% 
      mutate(GEO_ABB = geo.abb, DOMAIN_ABB = domain$DOMAIN_ABB, OWN_ABB = own$OWN_ABB) %>%
      select(GEO_ABB, DOMAIN_ABB, OWN_ABB, IMPUTATION, everything())
    saveRDS(estimates,
            paste0(out.path, "NWOS_2018_ESTIMATES_", own$OWN_ABB, "_", domain$DOMAIN_ABB, "_", geo.abb, ".RDS"))
    rm(estimates)
    gc()
  }
}

