#' nwos_table
#'
#' @export
#' @details For area and cooperation rate tables see ...
#' tab.num = 3
#' nwos_table()
#' nwos_table_area()
#' nwos_table_coop()

nwos_urban_table <- function(tab.num,
                             data = table.data,
                             data.coop = coop.table.data,
                             stratum.abb = STRATUM_ABB,
                             stratum.name = STRATUM_NAME,
                             domain.abb = DOMAIN_ABB,
                             domain.name = DOMAIN_NAME,
                             year = YEAR) {

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

  geo.name <- data$GEO_NAME[1]

  #### COOP TABLE ###
  if(tab.num == "00") {
    #### Begin ####
    html.table.begin <- c("",
                          "<table>")

    #### Caption ####
    html.table.caption <- paste0("<caption>",
                                 "<a id=\"TABLE_00\">",
                                 "Table ", gsub("_", "\\_", data.coop$GEO_ABB, fixed = T), "-",
                                 "00",
                                 "</a>",
                                 " (", year, #"; ",
                                 # stratum.abb, ", ", domain.abb,
                                 ").&mdash;",
                                 "Sample size and cooperation rate for the Urban National Landowner Survey for ",
                                 data.coop$GEO_NAME[1], ", ", year, ".",
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

    COOP_RATE <- data.coop$I / (data.coop$I + data.coop$P + data.coop$R)
    SAMPLE <-   data.coop$I + data.coop$NC + data.coop$P + data.coop$R + data.coop$UN

    #### Rows ####
    html.table.rows <- c("<tbody>",
                         "<tr>",
                         "<td>", nwos_urban_table_number(SAMPLE), "</td>",
                         "<td>", nwos_urban_table_number(data.coop$NC + data.coop$UN), "</td>",
                         "<td>", nwos_urban_table_number(data.coop$R), "</td>",
                         "<td>", nwos_urban_table_number(data.coop$P), "</td>",
                         "<td>", nwos_urban_table_number(data.coop$I), "</td>",
                         "<td>", nwos_urban_table_number(COOP_RATE * 100, d = 1), "</td>",
                         "</tr>",
                         "</tbody>")

    #### End ###
    html.table.end <- c("</table>")

    #### Footnotes ####
    html.table.footnotes <-
      c(paste0("<sup>a</sup>Cooperation Rate = Complete Responses / ",
               "(Complete Responses + Partial Responses + Nonresponses).<br>"),
        "<br>",
        "<br>")
  }

  #### NON-COOP TABLES ####
  else{
    #### Begin ####
    html.table.begin <- c("",
                          "<table>")

    #### Caption ####
    html.table.caption <- paste0("<caption>",
                                 "<a id=\"TABLE_", data$TABLE_NUMBER[1], "\">",
                                 "Table ", gsub("_", "\\_", data$GEO_ABB[1], fixed = T), "-",
                                 data$TABLE_NUMBER[1],
                                 "</a>",
                                 " (", year, #"; ",
                                 # stratum.abb, ", ", domain.abb,
                                 ").&mdash;",
                                 "Estimated number of ",
                                 tolower(stratum.name),  " ownerships ",
                                 "by ", data$DESCRIPTION[1], ", " , geo.name, ", ", year, ".",
                                 "</caption>")

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
                                       nwos_urban_table_row,
                                       data = data)),
                         "</tbody>")

    #### End ###
    html.table.end <- c("</table>")

    #### Footnotes ####
    html.table.footnotes <- c(if(data$FOOTNOTE[1] %in% "NOT_MUTUAL") {
      c("<sup>a</sup> Categories are not mutually exclusive.<br>",
        "<sup>b</sup> SE = standard error.<br>")}
      else{"<sup>a</sup> SE = standard error.<br>"},
      "Note: Data may not add to totals due to rounding.<br>",
      "<br>",
      "<br>")
  }

  #### Combine ####
  c(html.table.begin, html.table.caption, html.table.header, html.table.rows, html.table.end, html.table.footnotes)
}
