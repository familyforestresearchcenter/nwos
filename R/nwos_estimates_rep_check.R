#' nwos_estimates_rep_check
#'
#' Check number of replicates
#'
#' @export
#'

nwos_estimates_rep_check <- function(file.dir = "DATA", stratum = "FFO", domain = "ONEPLUS") {
  files <- list.files(file.dir, pattern = ".RDS")
  files <- files[!files %in% c("QUEST_20200212.RDS", "REP_WEIGHTS_20200212.RDS")]
  rep.nums <- 0:1000
  x <- gsub(paste0(stratum, "_", domain, "_"), "", gsub(".RDS", "", files))
  rep.nums[!rep.nums %in% x]
}
