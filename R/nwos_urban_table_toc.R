#' nwos_table_toc
#' @export

nwos_urban_table_toc <- function(table.number,
                                 data = table.data,
                                 data.coop = coop.table.data,
                                 stratum.abb = STRATUM_ABB,
                                 stratum.name = STRATUM_NAME,
                                 domain.abb = DOMAIN_ABB,
                                 domain.name = DOMAIN_NAME,
                                 year = YEAR) {
  if(table.number == "00") {
    c(paste0("<a href=\"#TABLE_00\">",
             "Table ", gsub("_", "\\_", data.coop$GEO_ABB, fixed = T), "-",
             "00",
             " (", year,
             ").&mdash;",
             "Sample size and cooperation rate for the Urban National Landowner Survey for ",
             data.coop$GEO_NAME[1], ", ", year, ".",
             "</a>"),
      "<br>")
  }
  else{
    toc.data <- data %>% filter(TABLE_NUMBER %in% table.number)

    c(paste0("<a href=\"#TABLE_", toc.data$TABLE_NUMBER[1], "\">",
             "Table ", gsub("_", "\\_", toc.data$GEO_ABB[1], fixed = T), "-",
             toc.data$TABLE_NUMBER[1],
             " (", year,
             ").&mdash;",
             "Estimated number of ",
             tolower(stratum.name),  " ownerships ",
             "by ", toc.data$DESCRIPTION[1], ", " , toc.data$GEO_NAME[1], ", ", year, ".",
             "</a>"),
      "<br>")
  }
}
