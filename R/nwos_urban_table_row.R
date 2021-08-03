#' nwos.urban.table.row
#'
#' Create the body of an NWOS table
#' @details For area and cooperation rate tables see ...
#' nwos.table.row(data.row = data[1,])
#' @export

nwos_urban_table_row <- function(i, data) {
  data.i <- data %>% slice(i)
  c("<tr>",
    paste0("<td>", data.i$LABEL, "</td>"),
    paste0("<td>", nwos_urban_table_number(data.i$OWN, r = -3, n = data.i$N), "</td>"),
    paste0("<td>", nwos_urban_table_number(data.i$OWN_SE, r = -3, n = data.i$N), "</td>"),
    paste0("<td>", nwos_urban_table_number(data.i$OWN_PERC, d = 1, n = data.i$N), "</td>"),
    paste0("<td>", nwos_urban_table_number(data.i$OWN_PERC_SE, d = 1, n = data.i$N, n.max = 145), "</td>"),
    paste0("<td>", nwos_urban_table_number(data.i$N, less.one = F), "</td>"),
    "</tr>")
  }
