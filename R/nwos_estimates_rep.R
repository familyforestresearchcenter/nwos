#' NWOS Estimates Total by State, Variable, and Level
#'
#' Estimator used to calculate NWOS totals by state, variable and level.
#' @usage nwos_total_svl(state, variable, level, data = QUEST, area = NA, weights = "WEIGHT")
#' @param state = "1"
#' @param variable = "HOME"
#' @param variable
#' @param level = "1",
#' @param data = QUEST_WIDE
#' @param area = NA
#' @param weights = "WEIGHT"
#' @keywords nwos
#' @details
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' rep = 1:2
#' variable.list = VARIABLE_LIST
#' quest.list = QUEST_LIST
#' rep.list = REPLICATES_LIST
#' geo.list = GEO_LIST
#' stratum = "FFO"
#' domain = "ONEPLUS"
#' g = 1
#' v = 1
#' x <- nwos_estimates_rep(variable.list = VARIABLE_LIST[1:5,], geo.list = GEO_LIST[1,])
#' nwos_estimates_rep(variable.list = VARIABLE_LIST[524,], geo.list = GEO_LIST[1,])
#' start.time <- Sys.time()
#' nwos_estimates_rep(geo.list = GEO_LIST[1,])
#' Sys.time() - start.time
#' start.time <- Sys.time()
#' x <- lapply(as.character(0:10), nwos_estimates_rep, variable.list = VARIABLE_LIST[1,], geo.list = GEO_LIST[1:2,])
#' Sys.time() - start.time
#' start.time <- Sys.time()
#' x <- mclapply(as.character(0:10), nwos_estimates_rep, variable.list = VARIABLE_LIST[1,], geo.list = GEO_LIST[1,], mc.cores = 3)
#' Sys.time() - start.time
#' bind_rows(mapply(variable = "TOTAL", level = 1, nwos_estimates_variable_imp_rep, SIMPLIFY = F))

nwos_estimates_rep <- function(rep = "0",
                               variable.list = VARIABLE_LIST,
                               quest.list = QUEST_LIST,
                               rep.list = REPLICATE_LIST,
                               geo.list = GEO_LIST,
                               stratum = "FFO",
                               domain = "ONEPLUS") {
  values <- tibble()

  for(i in 1:length(quest.list)) { # By imputation

    quest.imp <- rep.list[[rep]] %>% # By rep
      left_join(quest.list[[i]] %>% select(-WEIGHT), # By imp
                by = "CN") %>%
      filter(!!sym(stratum) == 1, !!sym(domain)) # Subset by stratum and domain

    for(g in 1:NROW(geo.list)) { # By GEO
      values.geo <- tibble()

      geo.cd <- unlist(strsplit(geo.list$GEO_CD[g], split=", "))
      geo.abb <- geo.list$GEO_ABB[g]

      quest.imp.geo <- quest.imp %>%
        filter(STATECD_NWOS %in% unlist(strsplit(geo.cd, split=", ")))

      for(v in 1:NROW(variable.list)) { # By Variable/Level
        values.variable <- tibble()
        variable <- variable.list[v,]

        if(variable$TYPE == "CATEGORICAL") { # Categorical variables

          quest.imp.geo.variable <- quest.imp.geo %>%
            mutate(VARIABLE = if_else(!!sym(variable$VARIABLE) %in% variable$LEVEL, 1, 0))

          values.variable <- bind_rows(values.variable,
                                       tibble(IMP = i, STATISTIC = "TOTAL", UNITS = "OWNERSHIPS",
                                              VALUE =   nwos_estimates_total(weight = quest.imp.geo.variable$WEIGHT,
                                                                             variable = quest.imp.geo.variable$VARIABLE)),
                                       tibble(IMP = i, STATISTIC = "TOTAL", UNITS = "ACRES",
                                              VALUE =   nwos_estimates_total(weight = quest.imp.geo.variable$WEIGHT,
                                                                             variable = quest.imp.geo.variable$VARIABLE,
                                                                             area = quest.imp.geo.variable$AC_WOOD)))

          if(rep == "0" & i == 1) { # Base
            values.variable <- bind_rows(values.variable,
                                         tibble(STATISTIC = "N", VALUE = sum(quest.imp.geo.variable$VARIABLE))) # n
          }
        }

        if(variable$TYPE == "CONTINUOUS") { # Continuous variables
          values.variable <- bind_rows(values.variable,
                                       tibble(IMP = i, STATISTIC = "MEAN", UNITS = "OWNERSHIPS",
                                              VALUE =  nwos_estimates_mean(weight = quest.imp.geo$WEIGHT,
                                                                           variable = quest.imp.geo[[variable$VARIABLE]])),
                                              tibble(IMP = i, STATISTIC = "MEDIAN", UNITS = "OWNERSHIPS",
                                                     VALUE = nwos_estimates_quantile(weight = quest.imp.geo$WEIGHT,
                                                                                     variable = quest.imp.geo[[variable$VARIABLE]],
                                                                                     p = 0.5)))
        }

        values.geo <- values.geo %>%
          bind_rows(values.variable %>% mutate(GEO = geo.abb,
                                               VARIABLE = variable$VARIABLE,
                                               LEVEL = variable$LEVEL))
      } # End Variable/Level
      values <- values %>%
        bind_rows(values.geo) %>%
        mutate(REP = rep) %>%
        select(GEO, VARIABLE, LEVEL, IMP, REP, STATISTIC, UNITS, VALUE)
    } # End GEO
  } # End imp
  return(values)
}
