#' NWOS Estimates Total by State, Variable, and Level
#'
#' Estimator used to calculate NWOS totals by state, variable and level.
#' @usage nwos_total_svl(state, variable, level, data = QUEST, area = NA, weights = "WEIGHT")
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' rep = "0"
#' variable.list = VARIABLE_LIST
#' quest.list = QUEST_LIST
#' rep.list = REPLICATE_LIST
#' geo.list = GEO_LIST
#' stratum = "FFO"
#' domain = "TENPLUS"
#' i = 1
#' g = 1
#' v = 1
#' start.time <- Sys.time()
#' nwos_estimates_rep(geo.list = GEO_LIST[1,])
#' Sys.time() - start.time # Time difference of 11.77129 secs
#' #' start.time <- Sys.time()
#' nwos_estimates_rep(geo.list = GEO_LIST[1,])
#' Sys.time() - start.time # Time difference of 11.77129 secs
#' nwos_estimates_rep(geo.list = GEO_LIST[55,], variable.list = VARIABLE_LIST[525,], write = F)

nwos_estimates_rep_geo <- function(rep = "0",
                               variable.list = VARIABLE_LIST,
                               quest.list = QUEST_LIST,
                               rep.list = REPLICATE_LIST,
                               geo.list = GEO_LIST,
                               stratum = "FFO",
                               domain = "TENPLUS",
                               calc.mean = T,
                               calc.median = T,
                               write = T) {

  for(i in 1:length(quest.list)) { # By imputation
    quest.imp <- rep.list[[rep]] %>% # By rep
      left_join(quest.list[[i]] %>% select(-WEIGHT), # By imp
                by = "CN") %>%
      filter(!!sym(stratum) == 1, !!sym(domain)) # Subset by stratum and domain

    for(g in 1:NROW(geo.list)) { # By GEO
      values.geo <- tibble()

      geo.cd <- unlist(strsplit(geo.list$GEO_CD[g], split=", "))
      geo.abb <- geo.list$GEO_ABB[g]

      quest.imp.geo <- quest.imp %>% filter(STATECD_NWOS %in% geo.cd)

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
          if(calc.mean){
            values.variable <- bind_rows(values.variable,
                                         tibble(IMP = i, STATISTIC = "MEAN", UNITS = "OWNERSHIPS",
                                                VALUE =  nwos_estimates_mean(weight = quest.imp.geo$WEIGHT,
                                                                             variable = quest.imp.geo[[variable$VARIABLE]])))
          }
          if(calc.median) {
            values.variable <- bind_rows(values.variable,
                                         tibble(IMP = i, STATISTIC = "MEDIAN", UNITS = "OWNERSHIPS",
                                                VALUE = nwos_estimates_quantile(weight = quest.imp.geo$WEIGHT,
                                                                                variable = quest.imp.geo[[variable$VARIABLE]],
                                                                                p = 0.5)))
          }
          if(variable$VARIABLE == "OWNERS_NUMBER") {
            values.variable <- bind_rows(values.variable,
                                         tibble(IMP = i, STATISTIC = "TOTAL", UNITS = "OWNERS",
                                                VALUE =  nwos_estimates_total(weight = quest.imp.geo$WEIGHT,
                                                                              variable = quest.imp.geo$OWNERS_NUMBER)))
          }
        }

        values.geo <- values.geo %>%
          bind_rows(values.variable %>% mutate(GEO = geo.abb,
                                               VARIABLE = variable$VARIABLE,
                                               LEVEL = variable$LEVEL))
      } # End Variable/Level
      values.geo <- values.geo %>%
        mutate(REP = rep) %>%
        select(GEO, VARIABLE, LEVEL, IMP, REP, STATISTIC, UNITS, VALUE)
      saveRDS(values.geo, paste0("DATA/", stratum, "_", domain, "_", rep, "_", geo.abb, ".RDS"))
    } # End GEO
  } # End imp
  # if(write){
  #   saveRDS(values, paste0("DATA/", stratum, "_", domain, "_", rep, ".RDS"))
  #   rm(rep.list, quest.list, quest.imp, values)
  #   return()
  # }
  # else return(values)
  rm(rep.list, quest.list, quest.imp)
  gc(verbose = F)
}
