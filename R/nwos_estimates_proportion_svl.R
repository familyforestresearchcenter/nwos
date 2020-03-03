#' NWOS Proportions by S / V / L
#'
#' Calculate NWOS Proportions by S / V / L
#' @usage nwos_proportion_svl_replicates(data.value = own.rsvl.rep, data.total = own.rsvl.tot.rep)
#' @param data.value = own.rsvl.rep
#' @param data.total = own.rsvl.tot.rep
#' @keywords nwos
#' @details
#' @export
#' @examples
#' ??

nwos_proportion_svl <- function(data.value = tot.own.value.svl, data.total = tot.own.total.svl) {
  left_join(data.value, data.total, by = "STATE") %>%
    mutate(PROPORTION = VALUE / TOTAL) %>%
    pull(PROPORTION)
}
