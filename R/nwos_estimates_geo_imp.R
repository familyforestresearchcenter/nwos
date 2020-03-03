#' nwos_estimates_geo_imp
#'
#' Generate estimates by geography and imputation
#' This is functuion is designed to be called by nwos_estimates_geo
#'
#'
#' @return data frame
#'
#' @examples
#' nwos_estimates_geo_imp("1", 1)
#' nwos_estimates_geo_imp("1", 2)
#' mapply(nwos_estimates_geo_imp, 1, imp = 1:2, SIMPLIFY = F)
#'
#' @export

nwos_estimates_geo_imp <- function(geo.cd.list,
                                   imp,
                                   domain = "AC_WOOD >= 10",
                                   quest = QUEST,
                                   variables.categorcial = VARIABLES_CATEGORICAL,
                                   variables.continuous = VARIABLES_CONTINUOUS) {
  options(warn=-1)
  if(grepl(", ", geo.cd.list)) {
    geo.cd <- unlist(strsplit(geo.cd.list, split=", "))
    var.method <- "approx"
  }
  else {
    geo.cd <- geo.cd.list
    var.method <- "full"
  }

  quest.geo.imp <<- quest[[imp]] %>% filter(STATECD_NWOS %in% geo.cd)

  design.own <<- twophase(id = list(~ 1, ~ CN),
                         strata = list(~ STATECD_NWOS + COND_STATUS_CD + OWNCD_NWOS, NULL),
                         subset = ~ RESP,
                         probs = list(NULL, ~ PROB),
                         data = quest.geo.imp,
                         method = var.method)
  design.ac <<-  twophase(id = list(~ 1, ~ CN),
                         strata = list(~ STATECD_NWOS + COND_STATUS_CD + OWNCD_NWOS, NULL),
                         subset = ~ RESP,
                         probs = list(NULL, ~ PROB_AC),
                         data = quest.geo.imp,
                         method = var.method)

  bind_rows(do.call(rbind, mapply(nwos_estimates_categorical,
                                  variable.name = variables.categorcial$VARIABLE,
                                  level.name = variables.categorcial$LEVEL,
                                  domain.name = domain,
                                  SIMPLIFY = F)),
            do.call(rbind, mapply(variable.name = variables.continuous,
                                  nwos_estimates_continuous,
                                  domain.name = domain,
                                  SIMPLIFY = F))) %>%
    mutate(IMPUTATION = imp, GEO_CD = paste(geo.cd, collapse = ", ")) %>% select(GEO_CD, everything())
}
