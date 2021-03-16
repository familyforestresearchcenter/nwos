#' nwos_estimates_combine
#'
#' Combine rep output by rep_geo and geo
#'
#' @export
#'

nwos_estimates_combine <- function(stratum = "FFO", domain = "ONEPLUS", wd = "DATA", n.cores = 1) {
  require(tidyverse)
  require(parallel)
  # n.cores <- detectCores() - 1

  dir.create(paste0(wd, "/REP_GEO"), showWarnings = FALSE)

  files <- list.files(paste0(wd, "/REP"), pattern = ".RDS")
  files <- files[!files %in% c("QUEST_20200212.RDS", "REP_WEIGHTS_20200212.RDS")]

  data <- readRDS(paste0(wd, "/REP/",files[1]))
  geo.list <- data %>% distinct(GEO) %>% pull()

  write_geo_rep <- function(geo = "US") {
    data.geo <- data %>% filter(GEO %in% geo)
    rep <- data.geo %>% distinct(REP) %>% pull()
    saveRDS(data.geo, paste0(wd, "/REP_GEO/" , stratum, "_", domain, "_", rep, "_", geo, ".RDS"))
  }

  for(i in files) {
    print(i)
    # data <- readRDS(i)
    data <- readRDS(paste0(wd, "/REP/", i))

    invisible(mclapply(geo.list, write_geo_rep, mc.cores = n.cores))
    # invisible(lapply(geo.list, write_geo_rep))

  }

  files.geo.list <- list.files(paste0(wd, "/REP_GEO"), pattern = ".RDS", full.names = T)

  for(i in geo.list) { # 1:64 [55]
    print(i)
    files.geo <- files.geo.list[grep(paste0("_", i, ".RDS"), files.geo.list)]
    data <- bind_rows(mclapply(files.geo,
                               function(x) {readRDS(x) %>% mutate(REP = as.numeric(REP))},
                               mc.cores = n.cores))
    # data <- bind_rows(lapply(files.geo,
    #                            function(x) {readRDS(x) %>% mutate(REP = as.numeric(REP))}))
    saveRDS(data, paste0(wd, "/GEO/" , stratum, "_", domain, "_", i, ".RDS"))
  }
}
