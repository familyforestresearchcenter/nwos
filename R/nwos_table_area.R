#' nwos_table
#'
#' Create the body of an NWOS core, area, or cooperataion rate table
#' @export
#' @param data
#' @details For area and cooperation rate tables see ...
#' geo.abb = "AL"
#' data = AREA
#' yr = year
#' yr.range = year.range
#' ref.tab = REF_TABLE
#' ref.geo = REF_GEO
#' nwos_table()
#' nwos_table_area(geo.abb = "AL")
#' nwos_table_coop()

nwos_table_area <- function(geo.abb, data = AREA,
                            yr = year, yr.range = year.range,
                            ref.tab = REF_TABLE, ref.geo = REF_GEO) {
  ref.geo <- ref.geo %>% filter(GEO_ABB %in% geo.abb)
  ref.tab <- ref.tab %>% filter(TABLE %in% "FOREST_AREA")

  data <- data %>% filter(GEO_ABB %in% geo.abb)
  total.acres <- data %>% filter(OWNCAT %in% "ALL") %>% pull(ACRES)
  data <- data %>% mutate(PROPORTION = ACRES / total.acres)

  begin.tex <- c("\\pagebreak",
                 "\\begin{minipage}{6.5in}",
                 "\\raggedright",
                 paste0("\\hypertarget{", ref.tab$TABLE_NUMBER, "}{\\hspace{20 mm}}\\\\"),
                 paste0("\\bookmark[page=\\thepage,level=0]{Table ", ref.tab$TABLE_NUMBER, " -- ",
                        ref.tab$TABLE_NAME, "}"))

  caption <- paste0("{\\setlength\\textwidth{5in} \\noindent \\textbf{",
                    "Table ", ref.geo$GEO_ABB, "-",
                    ref.tab$TABLE_NUMBER," (", yr,")--",
                    "Estimated area of forest land by ownership category, ",
                    ref.geo$GEO_NAME, ", ", yr.range,
                    "}}\\\\")

  body <- c("\\begin{center}",
            "\\begin{tabular}{l rr r}",
            "\\toprule",
            "\\textbf{\\specialcell{Ownership\\\\category}} & \\textbf{Acres} & \\textbf{SE$^a$} & \\textbf{Percentage} \\\\",
            '\\midrule',
            "& \\multicolumn{2}{c}{\\textit{- - thousands - -}} & \\\\",
            "Private \\\\",
            "\\rowcolor{gray!25}",
            paste0("\\hspace{0.5em} Family  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "FAMILY") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNCAT %in% "FAMILY") %>% pull(VARIANCE)) / 1e3), "  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "FAMILY") %>% pull(PROPORTION) * 100, d = 1),
                   " \\\\"),
            paste0("\\hspace{0.5em} Corporate  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "CORPORATE") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNCAT %in% "CORPORATE") %>% pull(VARIANCE)) / 1e3), "  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "CORPORATE") %>% pull(PROPORTION) * 100, d = 1),
                   " \\\\"),
            "\\rowcolor{gray!25}",
            paste0("\\hspace{0.5em} Other private  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "OTHER_RPIVATE") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNCAT %in% "OTHER_RPIVATE") %>% pull(VARIANCE)) / 1e3), "  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "OTHER_RPIVATE") %>% pull(PROPORTION) * 100, d = 1),
                   " \\\\"),
            "\\cmidrule{2-4}",
            paste0("\\hspace{0.5em} Total private  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "PRIVATE") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNCAT %in% "PRIVATE") %>% pull(VARIANCE)) / 1e3), "  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "PRIVATE") %>% pull(PROPORTION) * 100, d = 1),
                   " \\\\"),
            "Tribal \\\\",
            "\\rowcolor{gray!25}",
            paste0("\\hspace{0.5em} Tribal  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "TRIBAL") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNCAT %in% "TRIBAL") %>% pull(VARIANCE)) / 1e3), "  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "TRIBAL") %>% pull(PROPORTION) * 100, d = 1),
                   " \\\\"),
            "Public \\\\",
            "\\rowcolor{gray!25}",
            paste0("\\hspace{0.5em} Federal  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "FEDERAL") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNCAT %in% "FEDERAL") %>% pull(VARIANCE)) / 1e3), "  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "FEDERAL") %>% pull(PROPORTION) * 100, d = 1),
                   " \\\\"),
            paste0("\\hspace{0.5em} State  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "STATE") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNCAT %in% "STATE") %>% pull(VARIANCE)) / 1e3), "  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "STATE") %>% pull(PROPORTION) * 100, d = 1),
                   " \\\\"),
            '\\rowcolor{gray!25}',
            paste0("\\hspace{0.5em} Local  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "LOCAL") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNCAT %in% "LOCAL") %>% pull(VARIANCE)) / 1e3), "  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "LOCAL") %>% pull(PROPORTION) * 100, d = 1),
                   " \\\\"),
            "\\cmidrule{2-4}",
            paste0("\\hspace{0.5em} Total public  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "PUBLIC") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNCAT %in% "PUBLIC") %>% pull(VARIANCE)) / 1e3), "  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "PUBLIC") %>% pull(PROPORTION) * 100, d = 1),
                   " \\\\"),
            "\\cmidrule{2-4}",
            "\\rowcolor{gray!25}",
            paste0("\\hspace{0.5em} Total  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "ALL") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNCAT %in% "ALL") %>% pull(VARIANCE)) / 1e3), "  & ",
                   nwos_table_number(data %>% filter(OWNCAT %in% "ALL") %>% pull(PROPORTION) * 100, d = 1),
                   " \\\\"),
            "\\bottomrule \\\\",
            "\\end{tabular}",
            "\\end{center}")

  footnotes <- c("\\vspace{-0.25in}",
                 "\\begin{center}",
                 "\\begin{minipage}[c]{3.25in}",
                 "{\\noindent \\raggedright \\hangindent=0.1in",
                 "$^a$ SE = standard error\\\\",
                 "Note: Data may not add to totals due to rounding",
                 "}",
                 "\\end{minipage}",
                 "\\end{center}")

  end.tex <- c("\\end{minipage}",
               "\\\\")

  c(begin.tex, caption, body, footnotes, end.tex)
}
