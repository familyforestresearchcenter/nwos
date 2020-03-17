#' nwos_estimates_summary
#'
#' Combine and ummarize Imputations
#'
#' @examples
#' domain = "FFO"
#' stratum = "ONEPLUS"
#' nwos_estimates_summary()
#'
#' @export
#'

nwos_estimates_combine <- function(domain = "FFO", stratum = "TENPLUS") {
  files <- list.files(paste0("DATA/", domain, "/", stratum, "/REP"), pattern = ".RDS", full.names = T)

  data <- readRDS(files[1])
  geo.list <- data %>% distinct(GEO) %>% pull()

  write_geo_rep <- function(geo) {
    data.geo <- data %>% filter(GEO %in% geo)
    rep <- data.geo %>% distinct(REP) %>% pull()
    saveRDS(data.geo, paste0("DATA/", domain, "/", stratum, "/REP_GEO/" ,
                             domain, "_", stratum, "_", rep, "_", geo, ".RDS"))
  }

  for(i in files) {
    print(i)
    data <- readRDS(i)
    invisible(sapply(geo.list, write_geo_rep))
  }

  files.geo.list <- list.files(paste0("DATA/", domain, "/", stratum, "/REP_GEO"), pattern = ".RDS", full.names = T)

  # If too large, may be able to convery to wide
  for(i in geo.list[55]) { # 1:64
    print(i)
    files.geo <- files.geo.list[grep(paste0("_", i, ".RDS"), files.geo.list)]
    data <- bind_rows(lapply(files.geo,
                             function(x) {readRDS(x) %>% mutate(REP = as.numeric(REP))}))
    saveRDS(data, paste0("DATA/", domain, "/", stratum, "/GEO/" ,
                         domain, "_", stratum, "_", i, ".RDS"))
  }

}
