#' nwos_estimates_summary
#'
#' Combine and ummarize Imputations
#'
#' @examples
#' library(tidyverse)
#' library(parallel)
#' list.files("/home/ubuntu/GEO")
#' domain = "FFO"
#' stratum = "ONEPLUS"
#' # Need to run nwos_estimates_combine first
#' GEO_LIST <- read.csv("GEO_LIST.csv", stringsAsFactors = F)
#' geo.abb <- GEO_LIST$GEO_ABB
#' nwos_estimates_summary(geo.abb[1])
#' N_CORES <- detectCores() - 1
#' mclapply(geo.abb, nwos_estimates_summary, mc.cores = N_CORES)
#'
#' @export
#'

nwos_estimates_summary <- function(geo.abb = "CT", stratum = "FFO", domain = "TENPLUS") {
  print(geo.abb)
  # data <- readRDS(paste0("DATA/", stratum, "/", domain, "/GEO/" ,
  #                        stratum, "_", domain, "_", geo.abb, ".RDS"))
  # data <- readRDS(paste0("/home/ubuntu/GEO/" ,
  #                        stratum, "_", domain, "_", geo.abb, ".RDS"))
  data <- readRDS(paste0("DATA/FFO/TENPLUS/SUPPLEMENT/GEO/" ,
                         stratum, "_", domain, "_", geo.abb, ".RDS"))
  
  m <- data %>% filter(!is.na(IMP)) %>% distinct(IMP) %>% count() %>% pull()

  # Add proportions
  total <- data %>%
    filter(VARIABLE == "TOTAL", STATISTIC == "TOTAL") %>%
    rename("TOTAL" = "VALUE") %>% select(-VARIABLE, -LEVEL)
  prop <- data %>%
    filter(STATISTIC == "TOTAL") %>%
    left_join(total, by = c("GEO", "IMP", "REP", "STATISTIC", "UNITS")) %>%
    mutate(PROPORTION = VALUE / TOTAL) %>%
    select(-VALUE, -TOTAL) %>% rename(VALUE = PROPORTION) %>%
    mutate(STATISTIC = "PROPORTION")
  data <- data %>% bind_rows(prop)

  estimates.mean <- data %>%
    filter(REP == "0") %>%
    select(-IMP, -REP) %>%
    group_by(GEO, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
    summarize_all(mean) %>%
    ungroup()

  estimates.var.within <- data %>%
    filter(REP != "0") %>%
    select(-REP) %>%
    group_by(GEO, VARIABLE, LEVEL, IMP, STATISTIC, UNITS) %>%
    summarize(VAR_IMP = var(VALUE)) %>%
    ungroup() %>%
    select(-IMP) %>%
    group_by(GEO, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
    summarize(VAR_WITHIN = mean(VAR_IMP)) %>%
    ungroup()

  estimates.var.between <- data %>%
    filter(REP == "0") %>%
    select(-IMP, -REP) %>%
    group_by(GEO, VARIABLE, LEVEL, STATISTIC, UNITS) %>%
    summarize(VAR_BETWEEN = var(VALUE)) %>%
    ungroup()

  estimates <- estimates.mean %>%
    left_join(estimates.var.within, by = c("GEO", "VARIABLE", "LEVEL", "STATISTIC", "UNITS")) %>%
    left_join(estimates.var.between,by = c("GEO", "VARIABLE", "LEVEL", "STATISTIC", "UNITS")) %>%
    mutate(VARIANCE = VAR_WITHIN + ((1 + (1 / m)) * VAR_BETWEEN),
           STRATUM = stratum,
           DOMAIN = domain) %>%
    select(GEO_ABB = GEO, STRATUM, DOMAIN, VARIABLE, LEVEL, STATISTIC, UNITS, VALUE, VARIANCE)

  estimates %>% filter(VARIABLE == "OWNERS_NUMBER")
  estimates %>% filter(VARIABLE == "CUT_FORESTER_2")
  estimates %>% filter(VARIABLE == "EASE_5YR_2") %>% distinct(LEVEL)

  # saveRDS(estimates, paste0("DATA/", stratum, "/", domain, "/SUMMARY/NWOS_2018_" ,
  #                           stratum, "_", domain, "_", geo.abb, ".RDS"))
  # saveRDS(estimates, paste0("DATA/NWOS_2018_" ,
  #                           stratum, "_", domain, "_", geo.abb, ".RDS"))
  saveRDS(estimates, paste0("DATA/FFO/TENPLUS/SUPPLEMENT/SUMMARY/NWOS_2018_" ,
                            stratum, "_", domain, "_", geo.abb, ".RDS"))
}
