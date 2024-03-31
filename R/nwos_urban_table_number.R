#' nwos_urban_table_number
#'
#' Converts units (e.g., to thousands) and rounds similar to base except it adds another digit for values less than 1
#' @param x value to be formatted
#' @param d
#' @param r
#' @param n
#' @details For area and cooperation rate tables see ...
#' nwos_urban_table_number(1100)
#' nwos_urban_table_number(1100, r = -3)
#' nwos_urban_table_number(900)
#' nwos_urban_table_number(900, r = -3)
#' nwos_urban_table_number(1.1, d = 1)
#' nwos_urban_table_number(0.9)
#' nwos_urban_table_number(0.9, d = 1)
#' nwos_urban_table_number(0)
#' nwos_urban_table_number(0, n = 0)
#' nwos_urban_table_number(NA)
#' @export

nwos_urban_table_number <- function(x, d = 0, r = 0, n = 1, n.max = Inf, less.one = T, n.zero = T) {
  case_when (
    length(x) == 0 | is.na(x) ~ "--",
    n.zero & (n == 0 | n == n.max)~ "--",
    less.one & x < (1 * 10^abs(r)) ~ paste0("<", formatC(1 * 10^abs(r), big.mark = ",", format = "f", digits = d)),
    d == 1 & r == 0 ~ formatC(round(x, 1), big.mark = ",", format = "f", digits = d),
    TRUE ~ formatC(round(x, r), big.mark = ",", format = "f", digits = d))
}
