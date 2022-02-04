#' nwos_table_set
#'
#' Create the body of an NWOS table
#' @details
#' For area and cooperation rate tables see ...
#' @examples
#' nwos_table_set(geo.abb = "BALT")
#' geo.abb = "BALT"
#' data = ESTIMATES
#' ref.geo = REF_GEO
#' ref.table = REF_TABLE
#' stratum = STRATUM
#' stratum.abb = STRATUM_ABB
#' stratum.name = STRATUM_NAME
#' domain = DOMAIN
#' domain.abb = DOMAIN_ABB
#' domain.name = DOMAIN_NAME
#' year = YEAR
#'
#' @export

nwos_urban_table_set <- function(geo.abb, # geo.abb = "BALT"
                                 data = ESTIMATES,
                                 data.coop = COOP,
                                 ref.geo = REF_GEO,
                                 ref.table = REF_TABLE,
                                 stratum = STRATUM,
                                 stratum.abb = STRATUM_ABB,
                                 stratum.name = STRATUM_NAME,
                                 domain = DOMAIN,
                                 domain.abb = DOMAIN_ABB,
                                 domain.name = DOMAIN_NAME,
                                 year = YEAR,
                                 out.file = "HTML/TEST.html") {
  #### Setup ####
  coop.table.data <- data.coop
  table.data <- nwos_urban_table_data(data %>% filter(GEO_ABB == geo.abb))

  geo.name <- as.character(ref.geo %>% filter(GEO_ABB %in% geo.abb) %>% pull(GEO_NAME))
  geo.cd <- as.character(ref.geo %>% filter(GEO_ABB %in% geo.abb) %>% pull(GEO_CD))
  geo.level <- as.character(ref.geo %>% filter(GEO_ABB %in% geo.abb) %>% pull(GEO_LEVEL))

  ####  Start ####
  start.html <- c("<!DOCTYPE html>",
                  "<html>",
                  "",
                  "<head>",
                  "<link rel=\"stylesheet\" href=\"styles.css\">",
                  paste0("<title>Urban National Landowner Survey - ",
                         geo.name, " ", year, "</title>"),
                  "<style>",
                  "table {",
                  "border-collapse: collapse;",
                  "margin: 25px 0;",
                  "font-size: 0.9em;",
                  "font-family: sans-serif;",
                  "min-width: 400px;",
                  "margin-bottom:-2px; <!-- This is a hack. -->",
                  "}",
                  # "table thead tr {",
                  # "background-color: #009879;",
                  # "color: #ffffff;",
                  # "text-align: left;",
                  # "}",
                  "table thead tr {",
                  "padding: 12px 15px;",
                  "border-top: 1px solid;",
                  "border-bottom: 1px solid;",
                  "}",
                  "table caption {",
                  "text-align: left;",
                  "}",
                  "table th{",
                  "padding: 12px 15px;",
                  "border-top: 1px solid;",
                  "}",
                  "table td {",
                  "padding: 12px 15px;",
                  "}",
                  "table tbody tr {",
                  "border-bottom: 1px solid #dddddd;",
                  "}",
                  "table tbody tr:nth-of-type(even) {",
                  "background-color: #f3f3f3;",
                  "}",
                  # "table tbody tr:first-of-type {",
                  # "border-bottom: 1px solid;",
                  # "}",
                  "table tbody tr:last-of-type {",
                  "border-bottom: 1px solid;",
                  "}",
                  "table tbody tr.active-row {",
                  "font-weight: bold;",
                  "color: #009879;",
                  "}",
                  "</style>",
                  "</head>",
                  "",
                  "<body>",
                  "")

  #### title ####
  title.html <- c("<h1>",
                  "USDA Forest Service<br>",
                  "Urban National Landowner Survey<br>",
                  paste0(geo.name, ", ", year, "<br>"),
                  "</h1>",
                  "<h2>",
                  "Summary Tables<br>",
                  paste0("Draft (", Sys.Date(), ")<br>"),
                  "</h2>",
                  "<br>")

  ##### Citation ####
  cite.html <- c(paste0("Suggested citation: USDA Forest Service. 2021. Urban Forest Ownerships of ",
                        geo.name, ", ", year, ": ",
                        "Results from the USDA Forest Service, Urban National Landwner Survey. ",
                        "Madison, WI: U.S. Department of Agriculture, Forest Service, Northern Research Station. ",
                        "[ADD DOI].<br>"),
                 "<br>",
                 paste0("For more information about the National Landowner Survey, visit: ",
                        "<a href=\"https://www.fia.fs.fed.us/nwos/\">www.fia.fs.fed.us/nwos</a>"))

  #### TOC ####
  toc.html <- c("<h2>",
                "List of Tables",
                "</h2>",
                unlist(lapply(ref.table %>% distinct(TABLE_NUMBER) %>% pull(),
                              nwos_urban_table_toc,
                              data = table.data,
                              data.coop = coop.table.data,
                              stratum.abb = STRATUM_ABB,
                              stratum.name = STRATUM_NAME,
                              domain.abb = DOMAIN_ABB,
                              domain.name = DOMAIN_NAME,
                              year = YEAR)),
                "<br>",
                "<br>")

  #### Tables ####
  # coop.table.html <- nwos_urban_table_coop(geo.abb, COOP)

  tables.html <- c("<h2>Tables</h2>",
                   unlist(lapply(ref.table %>% distinct(TABLE_NUMBER) %>% pull(),
                                 nwos_urban_table,
                                 data = table.data,
                                 data.coop = coop.table.data,
                                 stratum.abb = STRATUM_ABB,
                                 stratum.name = STRATUM_NAME,
                                 domain.abb = DOMAIN_ABB,
                                 domain.name = DOMAIN_NAME,
                                 year = YEAR)))
  tables.html[1:29]
  #### End ####
  end.html <- c("",
                "</body>",
                "</html>",
                "")

  #### Combine & Write ####
  html <- c(start.html, title.html, cite.html, toc.html, tables.html, end.html)
  writeLines(html, out.file)

  return()
}

