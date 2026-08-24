#' nwos_html_table_set
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
#' excludes = EXCLUDES
#' inc.ac = F
#'
#' @export

nwos_html_table_set <- function(geo.abb,
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
								 excludes = EXCLUDES,
                                 inc.ac = F) { #include acreage tables?
  #### Setup ####
  coop.table.data <- data.coop
  table.data <- nwos_html_table_data(data %>% filter(GEO_ABB == geo.abb))
  
  geo.name <- as.character(ref.geo %>% filter(GEO_ABB %in% geo.abb) %>% pull(GEO_NAME))
  #geo.cd <- as.character(ref.geo %>% filter(GEO_ABB %in% geo.abb) %>% pull(GEO_CD))
  #geo.level <- as.character(ref.geo %>% filter(GEO_ABB %in% geo.abb) %>% pull(GEO_LEVEL))

  ####  Start ####
  start.html <- c("<!DOCTYPE html>",
                  "<html>",
                  "",
                  "<head>",
                  "<link rel=\"stylesheet\" href=\"styles.css\">",
                  paste0("<title>National Woodland Owner Survey - ", stratum.name, " ownerships", " ",
                         geo.name, " ", year, " ", domain.name, "</title>"),
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
                  paste0("National Woodland Owner Survey - ", stratum.name, " Ownerships<br>"),
                  paste0(geo.name, ", ", year, ", ", domain.name, "<br>"),
                  "</h1>",
                  "<h2>",
                  "Summary Tables<br>",
                  paste0("Draft (", Sys.Date(), ")<br>"),
                  "</h2>",
                  "<br>")

  ##### Citation ####
  cite.html <- c(paste0("Suggested citation: USDA Forest Service. In Review. Family and Small Corporate Forest Ownerships of the United States, ", year, ": ",
                        "Results from the USDA Forest Service, National Woodland Owner Survey. ",
                        "Madison, WI: U.S. Department of Agriculture, Forest Service, Northern Research Station. ",
                        "[ADD DOI].<br>"),
                 "<br>",
                 paste0("For more information about the National Woodland Owner Survey, visit: ",
                        "<a href=\"https://research.fs.usda.gov/programs/nwos/\" target=\"_blank\">research.fs.usda.gov/programs/nwos</a>"))

  #### TOC ####
  toc.html <- c("<h2>",
                "<a id=\"#TOP\">",
                "List of Tables",
                "</a>",
                "</h2>",
				ifelse(is.na(excludes),"",paste0("<i>Note: Totals do not include ownerships or acreage from ",excludes,"</i><br><br>")),
                unlist(lapply(ref.table %>% distinct(TABLE_NUMBER) %>% pull(),
                              nwos_html_table_toc,
                              data = table.data,
                              data.coop = coop.table.data,
                              stratum.abb = STRATUM_ABB,
                              stratum.name = STRATUM_NAME,
                              domain.abb = DOMAIN_ABB,
                              domain.name = DOMAIN_NAME,
                              year = YEAR,
                              inc.ac = inc.ac)),
                "<br>",
                "<br>")

  #### Tables ####
  # coop.table.html <- nwos_urban_table_coop(geo.abb, COOP)

  tables.html <- c("<h2>Tables</h2>",
                   unlist(lapply(ref.table %>% distinct(TABLE_NUMBER) %>% pull(),
                                 nwos_html_table,
                                 data = table.data,
                                 data.coop = coop.table.data,
                                 stratum.abb = STRATUM_ABB,
                                 stratum.name = STRATUM_NAME,
                                 domain.abb = DOMAIN_ABB,
                                 domain.name = DOMAIN_NAME,
                                 year = YEAR,
                                 inc.ac = inc.ac)))
  
  #### End ####
  end.html <- c("",
                #"<h2>",
                #"References",
                #"</h2>",
                #"Butler, Brett J.; Butler, Sarah M.; Caputo, Jesse; Dias, Jacqueline; Robillard, Amanda; Sass, Emma M. 2021. Family forest ownerships of the United States, 2018: results from the USDA Forest Service, National Woodland Owner Survey. Gen. Tech. Rep. NRS-199.Madison, WI: U.S. Department of Agriculture, Forest Service, Northern Research Station. 52 p. [plus 4 appendixes] https://doi.org/10.2737/NRS-GTR-199.",
                "</body>",
                "</html>",
                "")

  #### Combine & Write ####
  html <- c(start.html, title.html, cite.html, toc.html, tables.html, end.html)
  #file <- paste("HTML/",fdir,"/TABLES_",stratum.abb,"_",geo.abb,"_",domain,"_",year,".html",sep="")
  file <- paste("HTML/",fdir,"/TABLES",ID,"-",year,"-",stratum.abb,"-",geo.abb,"-",domain,".html",sep="")
  writeLines(html, file)

  return()
}

