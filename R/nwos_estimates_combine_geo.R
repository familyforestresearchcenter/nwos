#' nwos_estimates_summary
#'
#' Combine and ummarize Imputations
#'
#' @examples
#' # Check for missings reps
#' files <- list.files("DATA/FFO/TENPLUS/REP_GEO", pattern = "_US.RDS")
#' rep.nums <- 0:1000
#' x <- gsub("FFO_TENPLUS_", "", gsub("_US.RDS", "", files))
#' rep.nums[!rep.nums %in% x]
#' domain = "FFO"
#' stratum = "ONEPLUS"
#' nwos_estimates_summary()
#'
#' @export
#'

nwos_estimates_combine_geo <- function(domain = "FFO", stratum = "TENPLUS") {
  require(tidyverse)
  require(parallel)
  N_CORES <- detectCores() - 1
  # files <- list.files(paste0("DATA/", domain, "/", stratum, "/REP"), pattern = ".RDS", full.names = T)
  # files <- list.files("/home/ubuntu/REP", full.names = T)
  # files <- list.files("/home/ubuntu/SUPP", full.names = T)
  files <- list.files("~/Dropbox (FFRC)/NWOS/ESTIMATION/ESTIMATES/DATA/FFO/TENPLUS/SUPPLEMENT/REP", full.names = T)

  data <- readRDS(files[1])
  geo.list <- data %>% distinct(GEO) %>% pull()

  write_geo_rep <- function(geo) {
    data.geo <- data %>% filter(GEO %in% geo)
    rep <- data.geo %>% distinct(REP) %>% pull()
    # saveRDS(data.geo, paste0("DATA/", domain, "/", stratum, "/REP_GEO/" ,
    #                          domain, "_", stratum, "_", rep, "_", geo, ".RDS"))
    # saveRDS(data.geo, paste0("REP_GEO/" ,
    #                          domain, "_", stratum, "_", rep, "_", geo, ".RDS"))
    saveRDS(data.geo, paste0("DATA/FFO/TENPLUS/SUPPLEMENT/REP_GEO/" ,
                             domain, "_", stratum, "_", rep, "_", geo, ".RDS"))
  }

  for(i in files) {
    print(i)
    data <- readRDS(i)
    invisible(mclapply(geo.list, write_geo_rep, mc.cores = N_CORES))
  }

  # files.geo.list <- list.files(paste0("DATA/", domain, "/", stratum, "/REP_GEO"), pattern = ".RDS", full.names = T)
  # files.geo.list <- list.files("REP_GEO", pattern = ".RDS", full.names = T)
  files.geo.list <- list.files("DATA/FFO/TENPLUS/SUPPLEMENT/REP_GEO", pattern = ".RDS", full.names = T)

  # If too large, may be able to convert to wide
  for(i in geo.list) { # 1:64 [55]
    print(i)
    files.geo <- files.geo.list[grep(paste0("_", i, ".RDS"), files.geo.list)]
    data <- bind_rows(mclapply(files.geo,
                             function(x) {readRDS(x) %>% mutate(REP = as.numeric(REP))},
                             mc.cores = N_CORES))
    # saveRDS(data, paste0("DATA/", domain, "/", stratum, "/GEO/" ,
    #                      domain, "_", stratum, "_", i, ".RDS"))
    # saveRDS(data, paste0("GEO/" ,
    #                      domain, "_", stratum, "_", i, ".RDS"))
    saveRDS(data, paste0("DATA/FFO/TENPLUS/SUPPLEMENT/GEO/" ,
                         domain, "_", stratum, "_", i, ".RDS"))
  }
}
