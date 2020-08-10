# Back transform nwos.wide to nwos.long
# library(tidyverse)
#
# nwos.wide <- readRDS("~/Dropbox (FFRC)/NWOS/ESTIMATION/ESTIMATES/DATA/FFO/TENPLUS/NWOS_2018_FFO_TENPLUS_WIDE.RDS")
# names(nwos.wide)

# nwos.long <- left_join(nwos.wide %>%
#                          select(GEO_ABB:LEVEL, AC, OWN, AC_PROP, OWN_PROP, N, MEAN, MEDIAN, OWNERS) %>%
#                          pivot_longer(cols = c(AC, OWN, AC_PROP, OWN_PROP, N, MEAN, MEDIAN, OWNERS),
#                                       names_to = "STATISTIC", values_to = "VALUE") %>%
#                          filter(!is.na(VALUE)) %>%
#                          mutate(UNITS = recode(STATISTIC,
#                                                "AC" = "ACRES",
#                                                "OWN" = "OWNERSHIPS",
#                                                "AC_PROP" = "ACRES",
#                                                "OWN_PROP" = "OWNERSHIPS",
#                                                "MEAN" = "OWNERSHIPS",
#                                                "MEDIAN" = "OWNERSHIPS",
#                                                "OWNERS"  = "OWNERS"),
#                                 STATISTIC = recode(STATISTIC,
#                                                    "AC" = "TOTAL",
#                                                    "OWN" = "TOTAL",
#                                                    "AC_PROP" = "PROPORTION",
#                                                    "OWN_PROP" = "PROPORTION",
#                                                    "MEAN" = "MEAN",
#                                                    "MEDIAN" = "MEDIAN",
#                                                    "OWNERS"  = "TOTAL")),
#                        nwos.wide %>%
#                          select(GEO_ABB:LEVEL, AC_VAR, OWN_VAR, AC_PROP_VAR, OWN_PROP_VAR, MEAN_VAR, VARIANCE, OWNERS_VAR) %>%
#                          pivot_longer(cols = c(AC_VAR, OWN_VAR, AC_PROP_VAR, OWN_PROP_VAR, MEAN_VAR, VARIANCE, OWNERS_VAR),
#                                       names_to = "STATISTIC", values_to = "VARIANCE") %>%
#                          filter(!is.na(VARIANCE)) %>%
#                          mutate(UNITS = recode(STATISTIC,
#                                                "AC_VAR" = "ACRES",
#                                                "OWN_VAR" = "OWNERSHIPS",
#                                                "AC_PROP_VAR" = "ACRES",
#                                                "OWN_PROP_VAR" = "OWNERSHIPS",
#                                                "MEAN_VAR" = "OWNERSHIPS",
#                                                "VARIANCE" = "OWNERSHIPS",
#                                                "OWNERS_VAR"  = "OWNERS"),
#                                 STATISTIC = recode(STATISTIC,
#                                                    "AC_VAR" = "TOTAL",
#                                                    "OWN_VAR" = "TOTAL",
#                                                    "AC_PROP_VAR" = "PROPORTION",
#                                                    "OWN_PROP_VAR" = "PROPORTION",
#                                                    "MEAN_VAR" = "MEAN",
#                                                    "VARIANCE" = "MEDIAN",
#                                                    "OWNERS_VAR"  = "TOTAL"),
#                                 LEVEL = as.character(LEVEL)),
#                        by = c("GEO_ABB", "STRATUM", "DOMAIN", "VARIABLE", "LEVEL", "STATISTIC", "UNITS")) %>%
#   select(GEO_ABB:LEVEL, STATISTIC, UNITS, VALUE, VARIANCE) %>%
#   distinct()
#
# # 4481340
# nwos.long
#
# saveRDS(nwos.long, "~/Dropbox (FFRC)/NWOS/ESTIMATION/ESTIMATES/DATA/FFO/TENPLUS/NWOS_2018_FFO_TENPLUS.RDS")
# ")
