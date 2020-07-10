#' nwos_estimates_combine
#'
#' Combine rep output by rep_geo and geo
#'
#' @export
#'

nwos_estimates_combine <- function(stratum = "FFO", domain = "ONEPLUS", wd = "DATA") {
  require(tidyverse)
  require(parallel)
  N_CORES <- detectCores() - 1
  # files <- list.files(paste0("DATA/", stratum, "/", domain, "/REP"), pattern = ".RDS", full.names = T)
  # files <- list.files("/home/ubuntu/REP", full.names = T)
  # files <- list.files("/home/ubuntu/SUPP", full.names = T)
  # files <- list.files("~/Dropbox (FFRC)/NWOS/ESTIMATION/ESTIMATES/DATA/FFO/TENPLUS/SUPPLEMENT/REP", full.names = T)

  files <- list.files(paste0(wd, "/REP"), pattern = ".RDS")
  files <- files[!files %in% c("QUEST_20200212.RDS", "REP_WEIGHTS_20200212.RDS")]

  data <- readRDS(paste0(wd, "/",files[1]))
  geo.list <- data %>% distinct(GEO) %>% pull()

  write_geo_rep <- function(geo = "US") {
    data.geo <- data %>% filter(GEO %in% geo)
    rep <- data.geo %>% distinct(REP) %>% pull()
    # saveRDS(data.geo, paste0("DATA/", stratum, "/", domain, "/REP_GEO/" ,
    #                          stratum, "_", domain, "_", rep, "_", geo, ".RDS"))
    # saveRDS(data.geo, paste0("REP_GEO/" ,
    #                          stratum, "_", domain, "_", rep, "_", geo, ".RDS"))
    # saveRDS(data.geo, paste0("DATA/FFO/TENPLUS/SUPPLEMENT/REP_GEO/" ,
    #                          stratum, "_", domain, "_", rep, "_", geo, ".RDS"))
    # saveRDS(data.geo, paste0("DATA/REP_GEO/" ,
    #                          stratum, "_", domain, "_", rep, "_", geo, ".RDS"))
    saveRDS(data.geo, paste0(wd, "/REP_GEO/" , stratum, "_", domain, "_", rep, "_", geo, ".RDS"))
  }

  for(i in files) {
    print(i)
    # data <- readRDS(i)
    data <- readRDS(paste0(wd, "/REP/", i))

    invisible(mclapply(geo.list, write_geo_rep, mc.cores = N_CORES))
  }

  # files.geo.list <- list.files(paste0("DATA/", stratum, "/", domain, "/REP_GEO"), pattern = ".RDS", full.names = T)
  # files.geo.list <- list.files("REP_GEO", pattern = ".RDS", full.names = T)
  # files.geo.list <- list.files("DATA/FFO/TENPLUS/SUPPLEMENT/REP_GEO", pattern = ".RDS", full.names = T)
  # files.geo.list <- list.files("DATA/REP_GEO", pattern = ".RDS", full.names = T)
  files.geo.list <- list.files(paste0(wd, "/REP_GEO"), pattern = ".RDS", full.names = T)

  # If too large, may be able to convert to wide
  for(i in geo.list) { # 1:64 [55]
    print(i)
    files.geo <- files.geo.list[grep(paste0("_", i, ".RDS"), files.geo.list)]
    data <- bind_rows(mclapply(files.geo,
                               function(x) {readRDS(x) %>% mutate(REP = as.numeric(REP))},
                               mc.cores = N_CORES))
    # data <- tibble()
    # for(j in files.geo) {
    #   print(j)
    #   data <- data %>%
    #     bind_rows(readRDS(j) %>% mutate(REP = as.numeric(REP)))
    # }

    # saveRDS(data, paste0("DATA/", stratum, "/", domain, "/GEO/" ,
    #                      stratum, "_", domain, "_", i, ".RDS"))
    # saveRDS(data, paste0("GEO/" ,
    #                      stratum, "_", domain, "_", i, ".RDS"))
    # saveRDS(data, paste0("DATA/FFO/TENPLUS/SUPPLEMENT/GEO/" ,
    #                      stratum, "_", domain, "_", i, ".RDS"))
    # saveRDS(data, paste0("DATA/GEO/" ,
    #                      stratum, "_", domain, "_", i, ".RDS"))
    saveRDS(data, paste0(wd, "/GEO/" , stratum, "_", domain, "_", i, ".RDS"))
  }
}
