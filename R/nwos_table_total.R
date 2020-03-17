#' nwos_table_total
#'
#' geo.abb = "US"
#' data = ESTIMATES_WIDE
#' ref.geo = REF_GEO
#' ref.tab = REF_TABLE
#' stratum = stratum.name
#' domain = domain.name
#' yr = year
#' yr.range = year.range
#' nwos_table_total("US")


nwos_table_total <- function(geo.abb, data = QUEST_EST, 
                             ref.geo = REF_GEO, ref.tab = REF_TABLE,
                             stratum = stratum.name, domain = domain.name,
                             yr = year, yr.range = year.range) { 
  ref.geo <- ref.geo %>% filter(GEO_ABB %in% geo.abb)
  ref.tab <- ref.tab %>% filter(TABLE %in% "TOTAL")
  data <- data %>% filter(GEO_ABB %in% geo.abb, VARIABLE %in% "TOTAL")
  dom <- "XX"
  if(domain %in% "AC_WOOD >= 1") dom <- "1\\texttt{+}"
  if(domain %in% "AC_WOOD >= 10") dom <- "10\\texttt{+}"
  if(domain %in% "AC_WOOD >= 100") dom <- "100\\texttt{+}"
  if(domain %in% "AC_WOOD >= 1000") dom <- "1000\\texttt{+}"
  
  begin.tex <- c("\\pagebreak",
                 "\\begin{minipage}{6.5in}",
                 "\\raggedright",
                 paste0("\\hypertarget{", ref.tab$TABLE_NUMBER, "}{\\hspace{20 mm}}\\\\"),
                 paste0("\\bookmark[page=\\thepage,level=0]{Table ", ref.tab$TABLE_NUMBER, " -- ",
                        ref.tab$TABLE_NAME, "}"))
  
  caption <- paste0("{\\setlength\\textwidth{5in} \\noindent \\textbf{",
                    "Table ", ref.geo$GEO_ABB, "-", 
                    ref.tab$TABLE_NUMBER," (", yr, "; FFO ", dom, ")--",
                    "Total estimated area and estimated number of ", 
                    tolower(stratum),  " (", tolower(domain), ") ",
                    ", " , ref.geo$GEO_NAME, ", ", yr.range,
                    "}}\\\\")
  
  body <- c("\\begin{center}",
            "\\begin{tabular}{l rr rr r}",
            "\\toprule",
            "& \\textbf{Acres} & \\textbf{SE$^a$} & \\textbf{\\specialcell{Owner-\\\\ships}} & \\textbf{SE$^a$} & \\textbf{n}\\\\",
            "\\midrule",
            "& \\multicolumn{4}{c}{\\textit{- - - - - - - - thousands - - - - - - - }}&\\\\",
            paste0("Total & ", 
                   nwos_table_number(data$AC / 1e3), " & ", nwos_table_number(sqrt(data$AC_VAR) / 1e3), " & ", 
                   nwos_table_number(data$OWN / 1e3), " & ", nwos_table_number(sqrt(data$OWN_VAR) / 1e3), " & ", 
                   nwos_table_number(data$N), " \\\\"),
            "\\bottomrule",
            "\\end{tabular}",
            "\\end{center}")
  
  footnotes <- c("\\vspace{-0.25in}",
                 "\\begin{center}",
                 "\\begin{minipage}[c]{3in}",
                 "{\\noindent \\raggedright \\hangindent=0.1in",
                 "$^a$ SE = standard error",
                 "}",
                 "\\end{minipage}",
                 "\\end{center}")
  
  end.tex <- c("\\end{minipage}",
               "\\\\")
  
  c(begin.tex, caption, body, footnotes, end.tex)
}
