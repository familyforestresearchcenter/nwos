#' nwos_html_table_row
#'
#' Create the body of an NWOS table
#' @details For area and cooperation rate tables see ...
#' nwos.table.row(data.row = data[1,])
#' @export

nwos_html_table_row <- function(i, data, ac=F) {
  data.i <- data %>% slice(i)
  if (ac==T){
    c("<tr>",
      paste0("<td>", data.i$LABEL, "</td>"),
      paste0("<td>", nwos_html_table_number(data.i$AC, r = -3, n = data.i$N), "</td>"),
      paste0("<td>", nwos_html_table_number(data.i$AC_SE, r = -3, n = data.i$N), "</td>"),
      paste0("<td>", nwos_html_table_number(data.i$AC_PERC, d = 1, n = data.i$N), "</td>"),
      paste0("<td>", nwos_html_table_number(data.i$AC_PERC_SE, d = 1, n = data.i$N), "</td>"),
      paste0("<td>", nwos_html_table_number(data.i$N, less.one = F), "</td>"),
      "</tr>")
  } else {
    c("<tr>",
      paste0("<td>", data.i$LABEL, "</td>"),
      paste0("<td>", nwos_html_table_number(data.i$OWN, r = -3, n = data.i$N), "</td>"),
      paste0("<td>", nwos_html_table_number(data.i$OWN_SE, r = -3, n = data.i$N), "</td>"),
      paste0("<td>", nwos_html_table_number(data.i$OWN_PERC, d = 1, n = data.i$N), "</td>"),
      paste0("<td>", nwos_html_table_number(data.i$OWN_PERC_SE, d = 1, n = data.i$N), "</td>"),
      paste0("<td>", nwos_html_table_number(data.i$N, less.one = F), "</td>"),
      "</tr>")
  }
}
