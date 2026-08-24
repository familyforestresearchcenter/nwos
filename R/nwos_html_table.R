#' nwos_html_table
#'
#' @export
#' @details For area and cooperation rate tables see ...
#' tab.num = 3
#' nwos_table()
#' nwos_table_area()
#' nwos_table_coop()

nwos_html_table <- function(tab.num,
                             data = table.data,
                             data.coop = coop.table.data,
                             stratum.abb = STRATUM_ABB,
                             stratum.name = STRATUM_NAME,
                             domain.abb = DOMAIN_ABB,
                             domain.name = DOMAIN_NAME,
                             year = YEAR,
                             inc.ac = F) {

  geo.name <- data$GEO_NAME[1]
  geo.abb <- data$GEO_ABB[1]
  
  # data.cat <- data[["cat"]] %>%
  data <- data %>%
    filter(TABLE_NUMBER %in% tab.num,
           !is.na(LABEL), !LABEL %in% "") %>%
    mutate(HEADER = if_else(FOOTNOTE %in% "NOT_MUTUAL", paste0(HEADER, "<sup>a</sup>"), HEADER)) %>%
    arrange(SUBTABLE_NUMBER, ORDER)
  # data.cont <- data[["cont"]] %>% filter(TABLE_NUMBER %in% tab.num)
  tab <- data %>% select(TABLE) %>% distinct() %>% pull()
  sub <- data %>% select(SUBTABLE_NUMBER) %>% distinct() %>% pull()
  h.sub <- data %>% select(HEADER) %>% distinct() %>% pull()
  h <- data %>% select(HEADER) %>% distinct() %>% pull()
  foot <- data %>% select(FOOTNOTE) %>% distinct() %>% pull()

  #### COOP /AREA TABLES ###
  if(tab.num == "1a") {
    #### Begin ####
    html.table.begin <- c("",
                          "<table>")
    
    #### Caption ####
    html.table.caption <- paste0("<caption>",
                                 "<a id=\"TABLE_1a\">",
                                 "Table ", geo.abb, "-",
                                 "1a",
                                 "</a>",
                                 " (", year, #"; ",
                                 # stratum.abb, ", ", domain.abb,
                                 ").&mdash;",
                                 "Estimated area of forest land by ownership category, ",
                                 geo.name, ", ", year, ", 1+ acres.",
                                 "</caption>")
    
    #### Header ####
    html.table.header <- c("<thead>",
                           "<tr>",
                           "<th>Ownership group</th>",
                           "<th>Ownership class</th>",
                           "<th>Acres</th>",
                           "<th>SE<sup>a</sup></th>",
                           "</tr>",
                           "</thead>")
    
    #reduce forest area to geography
    GEO.FA <- REF_FOREST_AREA[REF_FOREST_AREA$GEO_ABB==geo.abb,]

    #function for formatting table row
    area_row <- function(x){
      c("<tr>",
        "<td>", GEO.FA$OWN.GRP[x], "</td>",
        "<td>", GEO.FA$OWN.CLS[x], "</td>",
        "<td>", nwos_urban_table_number(GEO.FA$ACRES[x],r = -3), "</td>",
        "<td>", nwos_urban_table_number(GEO.FA$ACRES_SE[x],r = -3), "</td>",
        "</tr>")
    }
    
    #### Rows ####
    html.table.rows <- c("<tbody>",
                         unlist(lapply(1:NROW(GEO.FA),
                                       area_row)),
                         "</tbody>")
    
    #### End ###
    html.table.end <- c("</table>")
    
    #### Footnotes ####
    html.table.footnotes <-
      c(paste0("<sup>a</sup> SE = standard error.<br>"),
        "<br>",
        "<a href=\"#TOP\">Back to Top</a>","<br>","<br>")
  } else if(tab.num == "1b") {
    #### Begin ####
    html.table.begin <- c("",
                          "<table>")
    
    #### Caption ####
    html.table.caption <- paste0("<caption>",
                                 "<a id=\"TABLE_1b\">",
                                 "Table ", data.coop$GEO_ABB, "-",
                                 "1b",
                                 "</a>",
                                 " (", year, #"; ",
                                 # stratum.abb, ", ", domain.abb,
                                 ").&mdash;",
                                 "Sample size and cooperation rate for ",tolower(stratum.name)," ownerships for the USDA Forest Service, National Woodland Owner Survey, ",
                                 geo.name, ", ", year, ", 1+ acres.",
                                 "</caption>")
    
    #### Header ####
    html.table.header <- c("<thead>",
                           "<tr>",
                           "<th>Sample size (n)</th>",
                           "<th>No/insufficient contact information (n)</th>",
                           "<th>Nonresponses (n)</th>",
                           "<th>Partial responses (n)</th>",
                           "<th>Complete responses (n)</th>",
                           "<th>Cooperation rate (percent)<sup>a</sup></th>",
                           "</tr>",
                           "</thead>")
    
    COOP_RATE <- data.coop$COOP_RATE
    SAMPLE <-   data.coop$I + data.coop$NC + data.coop$P + data.coop$R + data.coop$UN
    
    #### Rows ####
    html.table.rows <- c("<tbody>",
                         "<tr>",
                         "<td>", nwos_urban_table_number(SAMPLE), "</td>",
                         "<td>", nwos_urban_table_number(data.coop$NC + data.coop$UN, less.one=F), "</td>",
                         "<td>", nwos_urban_table_number(data.coop$R, less.one=F), "</td>",
                         "<td>", nwos_urban_table_number(data.coop$P, less.one=F), "</td>",
                         "<td>", nwos_urban_table_number(data.coop$I, less.one=F), "</td>",
                         "<td>", nwos_urban_table_number(COOP_RATE * 100, d = 1), "</td>",
                         "</tr>",
                         "</tbody>")
    
    #### End ###
    html.table.end <- c("</table>")
    
    #### Footnotes ####
    html.table.footnotes <-
      c(paste0("<sup>a</sup> Cooperation Rate = Complete Responses / ",
               "(Complete Responses + Partial Responses + Nonresponses).<br>"),
        "<br>",
        "<a href=\"#TOP\">Back to Top</a>","<br>","<br>")
  }

  #### OTHER TABLES ####
  else{
    #### Begin ####
    html.table.begin <- c("",
                          "<table>")

    #### Caption ####
    html.table.caption <- paste0("<caption>",
                                 "<a id=\"TABLE_", data$TABLE_NUMBER[1], "_OWN\">",
                                 "Table ", data$GEO_ABB[1], "-",
                                 data$TABLE_NUMBER[1],
                                 "-OWN",
                                 "</a>",
                                 " (", year, #"; ",
                                 # stratum.abb, ", ", domain.abb,
                                 ").&mdash;",
                                 "Estimated number of ",
                                 tolower(stratum.name),  " ownerships ",
                                 "by ", data$DESCRIPTION[1], ", " , geo.name, ", ", year, ", ", domain.name, ".",
                                 "</caption>")
    html.table.caption <- gsub(" by NA","",html.table.caption) #remove 'by NA' for totals

    #### Header ####
    html.table.header <- c("<thead>",
                           "<tr>",
                           paste0("<th>", data$HEADER[1], "</th>"),
                           "<th>Ownerships</th>",
                           if(data$FOOTNOTE[1] %in% "NOT_MUTUAL") {"<th>SE<sup>b</sup></th>"}
                           else {"<th>SE<sup>a</sup></th>"},
                           "<th>Percentage</th>",
                           if(data$FOOTNOTE[1] %in% "NOT_MUTUAL") {"<th>SE<sup>b</sup></th>"}
                           else {"<th>SE<sup>a</sup></th>"},
                           "<th>n</th>",
                           "</tr>",
                           "</thead>")

    #### Rows ####
    html.table.rows <- c("<tbody>",
                         unlist(lapply(1:NROW(data),
                                       nwos_html_table_row,
                                       data = data)),
                         "</tbody>")

    #### End ###
    html.table.end <- c("</table>")

    #### Footnotes ####
    if(data$FOOTNOTE[1] %in% "MEANMEDIAN") { #get mean / median from estimates
      root <- gsub("_CAT","",data$VARIABLE[1])
      root.data <- ESTIMATES %>% filter(GEO_ABB == GEO,
                                        VARIABLE == root) 
      rmean <- nwos_urban_table_number(round(root.data$MEAN,1))
      rmeanse <- nwos_urban_table_number(round(sqrt(root.data$MEAN_VAR),1))
      rmedian <- nwos_urban_table_number(root.data$MEDIAN)
      rmedian <- ifelse(rmedian=='<1','0',rmedian) #medians can be zero
    }
    html.table.footnotes <- c(if(data$FOOTNOTE[1] %in% "NOT_MUTUAL") {
      c("<sup>a</sup> Categories are not mutually exclusive.<br>",
        "<sup>b</sup> SE = standard error.<br>")}
      else if(data$FOOTNOTE[1] %in% "MEANMEDIAN") {
        c("<sup>a</sup> SE = standard error.<br>",
          paste("The average (mean) value per ownership is ",rmean," (SE = ",rmeanse,"); median is ",rmedian,".<br>",sep=""))}
      else{"<sup>a</sup> SE = standard error.<br>"},
      "Note: Data may not add to totals due to rounding.<br>",
      "<br>",
      "<a href=\"#TOP\">Back to Top</a>","<br>","<br>")
  }

  #### Combine ####
  tab <- c(html.table.begin, html.table.caption, html.table.header, html.table.rows, html.table.end, html.table.footnotes)
  
  if (inc.ac == T & !tab.num %in% c("1a","1b")){ #add acreage table by repurposing code chuncks, if option selected and not coop table
    
    #### Caption ####
    html.table.caption <- gsub("OWN","ACRE",html.table.caption) #change caption
    html.table.caption <- gsub("number of ","",html.table.caption)
    html.table.caption <- gsub("ownerships","acreage",html.table.caption)
    
    #### Header ####
    html.table.header[4] <- "<th>Acres</th>" #Acres in header
    
    #### Rows ####
    html.table.rows <- c("<tbody>",
                         unlist(lapply(1:NROW(data),
                                       nwos_html_table_row,
                                       data = data,
                                       ac = T)),
                         "</tbody>")
    
    #### Footnotes ####
    html.table.footnotes <- c(if(data$FOOTNOTE[1] %in% "NOT_MUTUAL") {
      c("<sup>a</sup> Categories are not mutually exclusive.<br>",
        "<sup>b</sup> SE = standard error.<br>")}
      else{"<sup>a</sup> SE = standard error.<br>"},
      "Note: Data may not add to totals due to rounding.<br>",
      "<br>",
      "<a href=\"#TOP\">Back to Top</a>","<br>","<br>")
    
    #### Combine ####
    tab <- c(tab,html.table.begin, html.table.caption, html.table.header, html.table.rows, html.table.end, html.table.footnotes)
    
  }
  
  return(tab)
}
