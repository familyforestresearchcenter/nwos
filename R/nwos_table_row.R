#' nwos.table.row
#'
#' Create the body of an NWOS table
#' @param GEO
#' @param TAB.NUM
#' @param TAB.TYPE table type. AREA = area by ownership category. COOP = cooperation rate. QUEST (Default) = questionnaire content.
#' @param POP . Default = "Family".
#' @param DOMAIN = NA
#' @param TABLE
#' @param YEARS Default = "2017-2018"
#' @details For area and cooperation rate tables see ...
#' nwos.table.row(data.row = data[1,])

nwos_table_row <- function(i, data) {
  paste0(if((i %% 2) == 1) "\\rowcolor{gray!25} ",
         "\\raggedright \\hspace*{1em} \\hangindent=1.4em ", data$LABEL[i], " & ", 
         nwos_table_number(data$AC[i]), " & ", nwos_table_number(data$AC_SE[i]), " & ", 
         nwos_table_number(data$OWN[i]), " & ", nwos_table_number(data$OWN_SE[i]), " & ", 
         nwos_table_number(data$AC_PERC[i]), " & ", nwos_table_number(data$AC_PERC_SE[i]), " & ",
         nwos_table_number(data$OWN_PERC[i]), " & ", nwos_table_number(data$OWN_PERC_SE[i]), " & ", 
         nwos_table_number(data$N[i]), " \\\\")}
