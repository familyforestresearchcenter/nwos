#' nwos_table
#'
#' Create the body of an NWOS core, area, or cooperataion rate table
#' @export
#' @details
#' nwos_table_coop("TX")

nwos_table_coop <- function(geo.abb,
                            data = COOP,
                            ref.geo = REF_GEO,
                            ref.tab = REF_TABLE,
                            year = YEAR,
                            year.range = YEAR_RANGE) {
  ref.geo <- ref.geo %>% filter(GEO_ABB %in% geo.abb)
  ref.tab <- ref.tab %>% filter(TABLE %in% "COOP_RATE")

  geo.name <- ifelse(ref.geo$GEO_ABB %in% c("US", "WEST", "PACIFIC_COAST"),
                     paste0(ref.geo$GEO_NAME, "$^{**}$"), ref.geo$GEO_NAME)

  data <- data %>% filter(GEO_ABB %in% geo.abb) %>% select(-GEO_CD)
  I <- data %>% filter(RESPONSE_CAT %in% "I") %>% pull(COUNT) # Complete surveys
  NC <- data %>% filter(RESPONSE_CAT %in% "NC") %>% pull(COUNT) # Non-contact
  P <- data %>% filter(RESPONSE_CAT %in% "P") %>% pull(COUNT) # Partial complete
  R <- data %>% filter(RESPONSE_CAT %in% "R") %>% pull(COUNT) # Refusal (nonresponse)
  UN <- data %>% filter(RESPONSE_CAT %in% "UN") %>% pull(COUNT) # Unknown
  COOP_RATE <- I / (I + P + R)
  SAMPLE <-   I + NC + P + R + UN

  geo.name <- ifelse(ref.geo$GEO_ABB %in% c("US", "WEST", "PACIFIC_COAST"),
                     paste0(ref.geo$GEO_NAME, "$^{*}$"), ref.geo$GEO_NAME)

  begin.tex <- c("\\pagebreak",
                 "\\begin{minipage}{6.5in}",
                 "\\raggedright",
                 paste0("\\hypertarget{", ref.tab$TABLE_NUMBER, "}{\\hspace{20 mm}}\\\\"),
                 paste0("\\bookmark[page=\\thepage,level=0]{Table ", ref.tab$TABLE_NUMBER, " -- ",
                        ref.tab$TABLE_NAME, "}"))

  caption <- paste0("{\\setlength\\textwidth{5in} \\noindent \\textbf{",
                    "Table ", gsub("_", "\\_", ref.geo$GEO_ABB, fixed = T), "-",
                    ref.tab$TABLE_NUMBER," (", year,"; FFO).--",
                    "Sample size and cooperation rate for family forest ownerships$^{*}$ ",
                    "for the USDA Forest Service, National Woodland Owner Survey, ",
                    geo.name, ", ", year.range,
                    "}}\\\\")

  body <- c("\\begin{center}",
            " \\begin{tabular}{ccccccc}",
            "\\toprule",
            paste0("\\textbf{\\specialcell{Sample\\\\size}} & \\textbf{\\specialcell{No/insufficient\\\\contact information}} &",
                   "\\textbf{Nonresponses} & \\textbf{\\specialcell{Partial\\\\responses}} & ",
                   "\\textbf{\\specialcell{Complete\\\\responses}} & ",
                   "\\textbf{\\specialcell{Cooperation\\\\rate$^a$}} \\\\"),
            "\\midrule",
            paste0("\\multicolumn{5}{c}{\\textit",
                   "{- - - - - - - - - - - - - - - - - - - - - - - - number ",
                   "- - - - - - - - - - - - - - - - - - - - - - - -}} & \\textit{percent} \\\\"),
            paste0(nwos_table_number(SAMPLE), " & ",
                   nwos_table_number(NC + UN), " & ",
                   # nwos_table_number(NC), " & ",
                   # nwos_table_number(UN), " & ",
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
                 ifelse(ref.geo$GEO_ABB %in% c("US", "WEST", "PACIFIC_COAST"), "$^{**}$ Excluding Interior Alaska. \\\\", ""),
                 "$^a$ $Cooperation Rate=\\frac{Complete Responses}{Complete Responses+Partial Responses+Nonresponses}$",
                 "}",
                 "\\end{minipage}",
                 "\\end{center}")

  end.tex <- c("\\end{minipage}",
               "\\\\")

  c(begin.tex, caption, body, footnotes, end.tex)
}
