#' nwos_table_set
#'
#' Create the body of an NWOS table
#' @param GEO
#' @param TAB_NUM
#' @param TAB_TYPE table type. AREA = area by ownership category. COOP = cooperation rate. QUEST (Default) = questionnaire content.
#' @param POP . Default = "Family".
#' @param DOMAIN = NA
#' @param TABLE
#' @param YEARS Default = "2017-2018"
#' @details For area and cooperation rate tables see ...
#' nwos_table_body()
#' nwos_table_body_area()
#' nwos_table_body_coop()
#' @examples 
#' nwos_table_set(geo.abb = "US")
#' geo.abb = "US"
#' data = ESTIMATES_WIDE
#' ref.geo = GEO_LIST
#' ref.table = REF_TABLE
#' stratum.name = "Family Forest Ownerships"
#' domain.name = "10\\texttt{+} acres"
#' year = "2018"
#' year.range = "2017-2018"
#' 

nwos_table_set <- function(geo.abb, 
                           data = ESTIMATES_WIDE,
                           ref.geo = GEO_LIST, 
                           ref.table = REF_TABLE,
                           stratum.name = "Family Forest Ownerships", 
                           domain.name = "10\\texttt{+} acres",
                           year = "2018", 
                           year.range = "2017-2018") {
  geo.name <- as.character(ref.geo %>% filter(GEO_ABB %in% geo.abb) %>% pull(GEO_NAME))
  # geo.abb <- as.character(ref.geo %>% filter(GEO_CD %in% geo.cd) %>% pull(GEO_ABB))
  
  #### tex.preamable #####
  tex.preamable <- c("\\documentclass{article}",
                     "\\usepackage[english]{babel}",
                     "\\renewcommand*{\\familydefault}{\\sfdefault}",
                     "\\renewcommand*\\rmdefault{arial}",
                     "\\usepackage[top=1in, bottom=1in, left=1in, right=1in]{geometry}",
                     "\\usepackage{amsmath}",
                     "\\usepackage[table]{xcolor}",
                     "\\definecolor{red}{rgb}{1,0,0}",
                     "\\usepackage{multirow}",
                     "\\usepackage{booktabs}",
                     "\\usepackage[hidelinks]{hyperref}",
                     "\\usepackage[all]{nowidow}",
                     "\\usepackage[numbered]{bookmark}",
                     "\\usepackage{fancyhdr}",
                     "\\renewcommand{\\headrulewidth}{0pt}",
                     "\\pagestyle{fancy}",
                     "\\makeatother",
                     "\\newcommand{\\specialcell}[2][c]{\\begin{tabular}[#1]{@{}c@{}}#2\\end{tabular}}",
                     ""
  )
  
  #### tex.begin ####
  tex.begin <- c("\\begin{document}",
                 "\\chead {\\Large \\textbf{\\textcolor{red}{-- DRAFT --\\\\-- Do Not Cite or Quote --}}}",
                 "\\cfoot {}",
                 paste0("\\rfoot{\\small ", Sys.Date(),"}"),
                 "\\raggedright")
  
  #### title ####
  title <- c("\\noindent \\Large \\textbf{USDA Forest Service \\vspace{.05in} \\\\",
             "National Woodland Owner Survey Summary Tables \\vspace{.05in} \\\\",
             paste0(stratum.name, " (", domain.name, ") \\vspace{.05in} \\\\"),
             if_else(geo.abb %in% c("US", "WEST", "PACIFIC_COAST"),
                     paste0(geo.name, "$^*$, ", year, "} \\vspace{.05in} \\\\"),
                     paste0(geo.name, ", ", year, "} \\vspace{.05in} \\\\")),
             if_else(geo.abb %in% c("US", "WEST", "PACIFIC_COAST"),
                     "\\normalsize $^*$ Exluding interior Alaska \\\\", ""),
             "\\bigskip",
             "\\bigskip",
             paste0("\\normalsize Suggested citation: Butler Brett J.; Butler, Sarah M.; Caputo, Jesse; Dias, Jaqueline; ", 
                    "Robillard, Amanda; Sass, Emma M. \\textcolor{red}{In review}. Family Forest Ownerships of the United States, 2018: ",
                    "Results from the USDA Forest Service, National Woodland Owner Survey. Res. Bull. NRS-\\textcolor{red}{XX}. ",
                    "Madison, WI: U.S. Department of Agriculture, Forest Service, Northern Research Station. ",
                    "\\textcolor{red}{[ADD DOI]} \\\\"),
             "\\bigskip",
             "\\bigskip",
             paste0("\\normalsize Separate sets of survey summary tables are available (as sample sizes permit) ",
                    "for the nation, regions, subregions, and states where the number of survey responses received ",
                    "meets or exceeds the minimum sample size of 100 (see Table 1 in the text of Res. Bull. ",
                    "NRS-\\textcolor{red}{XX}). The naming convention for tables is Table XX-\\texttt{\\#} (YYYY; SSS DD) ",
                    "where XX indicates the state, region, or nation abbreviation, \\texttt{\\#} indicates the table number, ",
                    "and YYYY indicates the nominal year of the data used to generate the results, SSS is the stratum of ",
                    "interest (FFO = family forest ownerships), and DD is domain of interest (e.g., 10\\texttt{+} acres of ",
                    "forest land).\\\\"),
             "\\bigskip",
             "\\bigskip")
  
  #### toc ####
  toc.data <- nwos_table_toc_data(geo.abb, stratum = stratum.name, domain = domain.name, yr = year, yr.range = year.range)
  toc <- lapply(unique(toc.data$TABLE_NUMBER), nwos_table_toc, toc.data, domain = domain.name)

  # #### tables ####
  table.area <- nwos_table_area(geo.abb, AREA, yr = year, yr.range = year.range)
  table.coop <- nwos_table_coop(geo.abb, COOP, yr = year, yr.range = year.range, stratum = stratum.name)
  table.total <- nwos_table_total(geo.abb, data, stratum = stratum.name, domain = domain.name, yr = year, yr.range = year.range)

  table.data <- nwos_table_data(data)
  nwos.tables <- lapply(unique(table.data$TABLE_NUMBER), nwos_table, table.data,
                        stratum = stratum.name, domain = domain.name, yr = year, yr.range = year.range)

  #### tex.end ####
  tex.end <- c("\\end{document}")

  c(tex.preamable, tex.begin, title, toc, table.area, table.coop, table.total, tex.end)
  c(tex.preamable, tex.begin, title, toc, table.area, table.coop, table.total, nwos.tables, tex.end)
}

