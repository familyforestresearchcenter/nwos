#' nwos_estimates_continuous
#'
#' Generate estimates for continuous variables
#' This is functuion is designed to be called by nwos_estimates_geo
#'
#' @param variable.name = "AC_WOOD",
#' @param domain.name = "AC_WOOD >= 10"
#' @return data frame
#'
#' @examples
#' nwos_estimates_continuous()
#'
#' @export

nwos_estimates_continuous <- function(variable.name = "AC_WOOD",
                                      domain.name = "AC_WOOD >= 10") {
  subset.own.name = paste0("subset(design.own, ", domain.name, ")")

  bind_rows(as_tibble(as.data.frame(svymean(~ eval(parse(text = variable.name)),
                                            eval(parse(text = subset.own.name))))) %>%
              mutate(STATISTIC = "MEAN", UNITS = "OWNERSHIPS", VARIANCE = SE^2) %>%
              rename(VALUE = 1) %>% select(-SE),
            as_tibble(as.data.frame(svyquantile(~ eval(parse(text = variable.name)),
                                                eval(parse(text = subset.own.name)), 0.5))) %>%
              mutate(STATISTIC = "MEDIAN", UNITS = "OWNERSHIPS") %>% rename(VALUE = 1)) %>%
    mutate(VARIABLE = variable.name) %>%
    select(VARIABLE, STATISTIC, UNITS, VALUE, VARIANCE)
}
