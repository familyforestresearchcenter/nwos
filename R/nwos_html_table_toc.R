#' nwos_html_table_toc
#' @export

nwos_html_table_toc <- function(table.number,
                                 data = table.data,
                                 data.coop = coop.table.data,
                                 stratum.abb = STRATUM_ABB,
                                 stratum.name = STRATUM_NAME,
                                 domain.abb = DOMAIN_ABB,
                                 domain.name = DOMAIN_NAME,
                                 year = YEAR,
                                 inc.ac = F) {
  
  if(table.number == "1a") {
    c(paste0("<a href=\"#TABLE_1a\">",
             "Table ", data$GEO_ABB[1], "-",
             "1a",
             " (", year,
             ").&mdash;",
             "Estimated area of forest land by ownership category, ",
             data$GEO_NAME[1], ", ", year, ", 1+ acres.",
             "</a>"),
      "<br>")
  } else if(table.number == "1b") {
    c(paste0("<a href=\"#TABLE_1b\">",
             "Table ", data.coop$GEO_ABB, "-",
             "1b",
             " (", year,
             ").&mdash;",
             "Sample size and cooperation rate for ",tolower(stratum.name)," ownerships for the USDA Forest Service, National Woodland Owner Survey, ",
             data$GEO_NAME[1], ", ", year, ", 1+ acres.",
             "</a>"),
      "<br>")
  }else{
    toc.data <- data %>% filter(TABLE_NUMBER %in% table.number)

    link <- paste0("<a href=\"#TABLE_", toc.data$TABLE_NUMBER[1], "_OWN\">",
             "Table ", toc.data$GEO_ABB[1], "-",
             toc.data$TABLE_NUMBER[1],
             "-OWN",
             " (", year,
             ").&mdash;",
             "Estimated number of ",
             tolower(stratum.name),  " ownerships ",
             "by ", toc.data$DESCRIPTION[1], ", " , toc.data$GEO_NAME[1], ", ", year, ", ", domain.name, ".",
             "</a>")
    link <- gsub(" by NA","",link) #remove 'by NA' for totals
    if (inc.ac==T){
      link2 <- gsub("OWN","ACRE",link)
      link2 <- gsub("number of ","",link2)
      link2 <- gsub("ownerships","acreage",link2)
      link <- c(link,"<br>",link2)
    }
    c(link,"<br>")
  }
}
