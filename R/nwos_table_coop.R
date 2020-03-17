#' nwos_table
#'
#' Create the body of an NWOS core, area, or cooperataion rate table
#' @param data
#' @details
#' geo.abb
#' data = COOP
#' ref.geo = REF_GEO
#' ref.tab = REF_TABLE
#' yr = year
#' yr.range = year.range
#' stratum = stratum.name
#' nwos_table_coop()

nwos_table_coop <- function(geo.abb,
                            data = COOP,
                            ref.geo = REF_GEO,
                            ref.tab = REF_TABLE,
                            yr = year,
                            yr.range = year.range,
                            stratum = stratum.name) {
  ref.geo <- ref.geo %>% filter(GEO_ABB %in% geo.abb)
  ref.tab <- ref.tab %>% filter(TABLE %in% "COOP_RATE")

  data <- data %>% filter(GEO_ABB %in% geo.abb) %>% select(-GEO_CD)
  I <- data %>% filter(RESPONSE_CAT %in% "I") %>% pull(COUNT) # Complete surveys
  NC <- data %>% filter(RESPONSE_CAT %in% "NC") %>% pull(COUNT) # Non-contact
  P <- data %>% filter(RESPONSE_CAT %in% "P") %>% pull(COUNT) # Partial complete
  R <- data %>% filter(RESPONSE_CAT %in% "R") %>% pull(COUNT) # Refusal (nonresponse)
  UN <- data %>% filter(RESPONSE_CAT %in% "UN") %>% pull(COUNT) # Unknown
  COOP_RATE <- I / (I + P + R)
  SAMPLE <-   I + NC + P + R + UN

  begin.tex <- c("\\pagebreak",
                 "\\begin{minipage}{6.5in}",
                 "\\raggedright",
                 paste0("\\hypertarget{", ref.tab$TABLE_NUMBER, "}{\\hspace{20 mm}}\\\\"),
                 paste0("\\bookmark[page=\\thepage,level=0]{Table ", ref.tab$TABLE_NUMBER, " -- ",
                        ref.tab$TABLE_NAME, "}"))

  caption <- paste0("{\\setlength\\textwidth{5in} \\noindent \\textbf{",
                    "Table ", ref.geo$GEO_ABB, "-",
                    ref.tab$TABLE_NUMBER," (", yr,"; FFO 1\\texttt{+})--",
                    "Sample size and cooperation rate for family forest ownerships$^{*}$ ",
                    "for the USDA Forest Service, National Woodland Owner Survey, ",
                    ref.geo$GEO_NAME, ", ", yr.range,
                    "}}\\\\")

  body <- c("\\begin{center}",
            " \\begin{tabular}{ccccccc}",
            "\\toprule",
            paste0("\\textbf{\\specialcell{Sample\\\\size}} & \\textbf{Non-contact} & \\textbf{Unknown} &",
                   "\\textbf{Nonresponses} & \\textbf{\\specialcell{Partial\\\\responses}} & ",
                   "\\textbf{\\specialcell{Complete\\\\responses}} & ",
                   "\\textbf{\\specialcell{Cooperation\\\\rate$^a$}} \\\\"),
            "\\midrule",
            paste0("\\multicolumn{6}{c}{\\textit",
                   "{- - - - - - - - - - - - - - - - - - - - - - - - number ",
                   "- - - - - - - - - - - - - - - - - - - - - - - -}} & \\textit{percent} \\\\"),
            paste0(nwos_table_number(SAMPLE), " & ",
                   nwos_table_number(NC), " & ",
                   nwos_table_number(UN), " & ",
                   nwos_table_number(R), " & ",
                   nwos_table_number(P), " & ",
                   nwos_table_number(I), " & ",
                   nwos_table_number(COOP_RATE * 100, d = 1), " \\\\"),
            "\\bottomrule \\\\",
            "\\end{tabular}",
            "\\end{center}")

  footnotes <- c("\\vspace{-0.25in}",
                 "\\begin{center}",
                 "\\begin{minipage}[c]{6in}",
                 "{\\noindent \\raggedright \\hangindent=0.1in",
                 "$^{*}$ These numbers are for all family forest ownerships with 1\\texttt{+} acres of forest land. \\\\",
                 "$^a$ $Cooperation Rate=\\frac{Complete Responses}{Complete Responses+Partial Responses+Nonresponses}$",
                 "}",
                 "\\end{minipage}",
                 "\\end{center}")

  end.tex <- c("\\end{minipage}",
               "\\\\")

  c(begin.tex, caption, body, footnotes, end.tex)
}
