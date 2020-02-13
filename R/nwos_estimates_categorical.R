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
#' nwos_estimates_categorical()
#' nwos_estimates_categorical(domain.name = "AC_WOOD >= 1")
#'
#' @export

nwos_estimates_categorical <- function(variable.name = "OWNTYPE",
                                       level.name = "1",
                                       domain.name = "AC_WOOD >= 10") {
  subset.own.name = paste0("subset(design.own, ", domain.name, ")")
  subset.ac.name = paste0("subset(design.ac, ", domain.name, ")")

  bind_rows(as_tibble(as.data.frame(svyby(~ ONE, ~ eval(parse(text = variable.name)) == level.name,
                                          eval(parse(text = subset.own.name)),
                                          svytotal, vartype="var"))[2, 2:3]) %>%
              rename(VALUE = 1, VARIANCE = 2) %>%
              mutate(STATISTIC = "TOTAL", UNITS = "OWNERSHIPS"),
            as_tibble(as.data.frame(svyby(~ ONE, ~ eval(parse(text = variable.name)) == level.name,
                                          eval(parse(text = subset.ac.name)),
                                          svytotal, vartype="var"))[2, 2:3]) %>%
              rename(VALUE = 1, VARIANCE = 2) %>%
              mutate(STATISTIC = "TOTAL", UNITS = "ACRES"),
            as_tibble(as.data.frame(svymean(~ eval(parse(text = variable.name)) == level.name,
                                            eval(parse(text = subset.own.name))))[2,]) %>%
              rename(VALUE = 1) %>%
              mutate(STATISTIC = "PROPORTION", UNITS = "OWNERSHIPS", VARIANCE = SE^2) %>%
              select(-SE),
            as_tibble(as.data.frame(svymean(~ eval(parse(text = variable.name)) == level.name,
                                            eval(parse(text = subset.ac.name))))[2,]) %>%
              rename(VALUE = 1) %>%
              mutate(STATISTIC = "PROPORTION", UNITS = "ACRES", VARIANCE = SE^2) %>%
              select(-SE),
            tibble(VALUE = quest.geo.imp %>% filter(!!sym(variable.name) == 1) %>% count() %>% pull(),
                   STATISTIC = "N", UNITS = "RESPONSES")) %>%
    mutate(VARIABLE = variable.name, LEVEL = level.name) %>%
    select(VARIABLE, LEVEL, STATISTIC, UNITS, VALUE, VARIANCE)
}
