#' nwos_table_set
#'
#' Create the body of an NWOS table
#' @details
#' For area and cooperation rate tables see ...
#' @examples
#' nwos_table_set(geo.abb = "US")
#' geo.abb = "US"
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
#' year.range = YEAR_RANGE
#' min.n = 100
#'
#' @export

nwos_table_set <- function(geo.abb, # geo.abb = "SOUTH"
                           data = ESTIMATES,
                           ref.geo = REF_GEO,
                           ref.table = REF_TABLE,
                           stratum = STRATUM,
                           stratum.abb = STRATUM_ABB,
                           stratum.name = STRATUM_NAME,
                           domain = DOMAIN,
                           domain.abb = DOMAIN_ABB,
                           domain.name = DOMAIN_NAME,
                           year = YEAR,
                           year.range = YEAR_RANGE,
                           min.n = 100) {

  data <- data %>% filter(GEO_ABB == geo.abb)

  if(data %>% filter(VARIABLE == "TOTAL") %>% pull(N) >= min.n) {

    # Convert
    geo.name <- as.character(ref.geo %>% filter(GEO_ABB %in% geo.abb) %>% pull(GEO_NAME))
    geo.cd <- as.character(ref.geo %>% filter(GEO_ABB %in% geo.abb) %>% pull(GEO_CD))
    geo.level <- as.character(ref.geo %>% filter(GEO_ABB %in% geo.abb) %>% pull(GEO_LEVEL))
    # geo.abb <- as.character(ref.geo %>% filter(GEO_CD %in% geo.cd) %>% pull(GEO_ABB))

    if(geo.level %in% c("REGION", "SUBREGION")) {
      ref.geo <- ref.geo %>% bind_rows(tibble(GEO_CD = c("2", "40", "48"), GEO_NAME = c("Alaska - Coastal", "Oklahoma", "Texas")))
      state.list <- ref.geo %>% filter(GEO_CD %in% as.integer(unlist(strsplit(geo.cd, ", ")))) %>%
        # mutate(GEO_NAME = if_else(GEO_CD %in% c(40.1, 40.2), "Oklahoma", GEO_NAME),
        #        GEO_NAME = if_else(GEO_CD %in% c(48.1, 48.2), "Texas", GEO_NAME)) %>%
        select(GEO_NAME) %>% distinct() %>% arrange(GEO_NAME) %>% pull()

      state.list <- paste0(paste(state.list[1:(NROW(state.list) - 1)], collapse = ", "),
                           ", and ", state.list[NROW(state.list)])
    }


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
                   # "\\chead {\\Large \\textbf{\\textcolor{red}{-- DRAFT --\\\\-- Do Not Cite or Quote --}}}",
                   "\\cfoot {}",
                   paste0("\\rfoot{\\small ", Sys.Date(),"}"),
                   "\\raggedright")

    #### title ####
    title <- c("\\noindent \\Large \\textbf{USDA Forest Service \\vspace{.05in} \\\\",
               "National Woodland Owner Survey Summary Tables \\vspace{.05in} \\\\",
               paste0(stratum.name, " (", domain.name, ") \\vspace{.05in} \\\\"),
               ifelse(geo.level %in% c("NATION", "REGION", "SUBREGION"),
                       paste0(geo.name, "$^*$, ", year, "} \\vspace{.05in} \\\\"),
                       paste0(geo.name, ", ", year, "} \\vspace{.05in} \\\\")),
               ifelse(geo.level %in% c("NATION"),
                      "\\normalsize $^*$ Exluding interior Alaska \\\\", ""),
               ifelse(geo.level %in% c("REGION", "SUBREGION"),
                      paste0("\\normalsize $^*$ Includes ", state.list, " \\\\"), ""),
               "\\bigskip",
               "\\bigskip",
               paste0("\\normalsize Suggested citation: Butler, Brett J.; Butler, Sarah M.; Caputo, Jesse; Dias, Jacqueline; ",
                      "Robillard, Amanda; Sass, Emma M. 2020. Family Forest Ownerships of the United States, 2018: ",
                      "Results from the USDA Forest Service, National Woodland Owner Survey. Gen. Tech. Rep. NRS-199. ",
                      "Madison, WI: U.S. Department of Agriculture, Forest Service, Northern Research Station. ",
                      "\\href{https://doi.org/10.2737/NRS-GTR-199}{https://doi.org/10.2737/NRS-GTR-199}. \\\\"),
               "\\bigskip",
               "\\bigskip",
               paste0("\\normalsize Separate sets of survey summary tables are available ",
                      "for the nation, regions, subregions, and states where the number of survey responses received ",
                      "meets or exceeds the minimum sample size of 100 (see Table 1 in the text of Gen. Tech. Rep. ",
                      "NRS-199). The naming convention for tables is Table XX-\\texttt{\\#} (YYYY; SSS, DDD) ",
                      "where XX indicates the state, region, or nation abbreviation, \\texttt{\\#} is the table number, ",
                      "YYYY is the nominal year of the NWOS cycle, SSS is the stratum of interest ",
                      "(FFO = family forest ownerships), and DDD is the domain of interest (1\\texttt{+}, ",
                      "10\\texttt{+}, 100\\texttt{+}, or 1,000\\texttt{+} acres of forest land).\\\\"),
               "\\bigskip",
               "\\bigskip")

    #### toc ####
    toc.data <- nwos_table_toc_data(geo.abb, stratum = stratum.name, domain = domain.name, yr = year, yr.range = year.range)
    toc <- lapply(unique(toc.data$TABLE_NUMBER), nwos_table_toc, toc.data, domain = domain.name, domain.abb = domain.abb)

    #### tables ####
    table.area <- nwos_table_area(geo.abb, AREA)
    table.coop <- nwos_table_coop(geo.abb, COOP)
    table.total <- nwos_table_total(geo.abb, data)

    table.data <- nwos_table_data(data)
    nwos.tables <- lapply(unique(table.data[["cat"]]$TABLE_NUMBER), nwos_table, table.data)

    #### tex.end ####
    tex.end <- c("\\end{document}")

    c(tex.preamable, tex.begin, title, toc, table.area, table.coop, table.total, nwos.tables, tex.end)
  }
}

