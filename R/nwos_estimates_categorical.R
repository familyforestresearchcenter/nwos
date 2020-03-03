#' nwos_estimates_categorical
#'
#' Generate estimates for categorical variables
#' This is functuion is designed to be called by nwos_estimates_geo
#'
#' @param variable.name = "MAN_PLAN",
#' @param level.name = "1",
#' @param domain.name = "AC_WOOD >= 10"
#' @return data frame
#'
#' @examples
#' nwos_estimates_categorical(geo.cd = "1", imp = 1)
#'
#' @export

nwos_estimates_categorical <- function(domain = "AC_WOOD >= 10",
                                       variable = "OWNTYPE",
                                       level = "1",
                                       design.own,
                                       design.ac,
                                       quest) {
  subset.own = paste0("subset(design.own, ", domain, ")")
  subset.ac = paste0("subset(design.ac, ", domain, ")")
  
  estimates <- bind_rows(as_tibble(as.data.frame(svyby(~ ONE, ~ eval(parse(text = variable)) == level,
                                                       eval(parse(text = subset.own)),
                                                       svytotal, vartype="var"))[2, 2:3]) %>%
                           rename(VALUE = 1, VARIANCE = 2) %>%
                           mutate(STATISTIC = "TOTAL", UNITS = "OWNERSHIPS"),
                         as_tibble(as.data.frame(svyby(~ ONE, ~ eval(parse(text = variable)) == level,
                                                       eval(parse(text = subset.ac)),
                                                       svytotal, vartype="var"))[2, 2:3]) %>%
                           rename(VALUE = 1, VARIANCE = 2) %>%
                           mutate(STATISTIC = "TOTAL", UNITS = "ACRES"),
                         as_tibble(as.data.frame(svymean(~ eval(parse(text = variable)) == level,
                                                         eval(parse(text = subset.own))))[2,]) %>%
                           rename(VALUE = 1) %>%
                           mutate(STATISTIC = "PROPORTION", UNITS = "OWNERSHIPS", VARIANCE = SE^2) %>%
                           select(-SE),
                         as_tibble(as.data.frame(svymean(~ eval(parse(text = variable)) == level,
                                                         eval(parse(text = subset.ac))))[2,]) %>%
                           rename(VALUE = 1) %>%
                           mutate(STATISTIC = "PROPORTION", UNITS = "ACRES", VARIANCE = SE^2) %>%
                           select(-SE),
                         tibble(VALUE = quest %>% filter(!!sym(variable) == 1) %>% count() %>% pull(),
                                STATISTIC = "N", UNITS = "RESPONSES")) %>%
    mutate(VARIABLE = variable, LEVEL = level) %>%
    select(VARIABLE, LEVEL, STATISTIC, UNITS, VALUE, VARIANCE)
  
  rm(design.own, design.ac)
  gc()
  
  return(estimates)
}
