#' nwos_round
#'
#' Converts units (e.g., to thousands) and rounds simialr to base except it adds another digit for values less than 1
#' @details For area and cooperation rate tables see ...
#' nwos_number_format(1100)
#' nwos_number_format(900)
#' @export

nwos_table_number <- function(x, d = 0, less.one = T) {
  if(less.one) {
    ifelse(x < 1, "$<$1",
           formatC(round(x, d), big.mark = ",", format = "f", digits = d))}
  else formatC(round(x, d), big.mark = ",", format = "f", digits = d)
}

