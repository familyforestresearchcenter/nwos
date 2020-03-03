#' NWOS Proportions by S / V / L for Replicates
#'
#' Calculate NWOS Proportions by S / V / L for Replicates
#' @usage nwos_proportion_svl_replicates(rep, data.value = own.rsvl.rep, data.total = own.rsvl.tot.rep)
#' @param rep = 1,
#' @param data.value = own.rsvl.rep
#' @param data.total = own.rsvl.tot.rep
#' @keywords nwos
#' @details
#' @export
#' @examples
#' ??

nwos_proportion_svl_replicates <- function(rep, data.value = own.rsvl.rep, data.total = own.rsvl.tot.rep) {
  data.value <- data.value %>% select(STATE, VARIABLE, LEVEL, VALUE = paste0("V", rep))
  data.total <- data.total %>% select(STATE, TOTAL = paste0("V", rep))
  nwos_proportion_svl(data.value, data.total)
}
