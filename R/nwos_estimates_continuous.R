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

nwos_estimates_continuous <- function(domain = "AC_WOOD >= 10",
                                      variable = "OWNTYPE",
                                      design.own) {
  subset.own = paste0("subset(design.own, ", domain, ")")

  estimates <- bind_rows(as_tibble(svymean(~ eval(parse(text = variable)),
                                            eval(parse(text = subset.own)))) %>%
              rename("VALUE" = 1, "SE" = 2) %>%
              mutate(STATISTIC = "MEAN", UNITS = "OWNERSHIPS", VARIANCE = SE^2) %>%
              select(-SE),
            as_tibble(svyquantile(~ eval(parse(text = variable)),
                                                eval(parse(text = subset.own)), 0.5)) %>%
              mutate(STATISTIC = "MEDIAN", UNITS = "OWNERSHIPS") %>% rename(VALUE = 1)) %>%
    mutate(VARIABLE = variable) %>%
    select(VARIABLE, STATISTIC, UNITS, VALUE, VARIANCE)
  
  rm(design.own)
  gc()
  
  return(estimates)
}
