#' nwos_table
#'
#' @export
#' @details For area and cooperation rate tables see ...
#' nwos_table()
#' nwos_table_area()
#' nwos_table_coop()

nwos_table <- function(tab.num, data = table.data,
                       stratum.abb = STRATUM_ABB, stratum.name = STRATUM_NAME,
                       domain.abb = DOMAIN_ABB, domain.name = DOMAIN_NAME,
                       year = YEAR, year.range = YEAR_RANGE) {
  # if() data.foot <- data
  data.cat <- data[["cat"]] %>% filter(TABLE_NUMBER %in% tab.num,
                          !is.na(LABEL), !LABEL %in% "") %>%
    mutate(LABEL = gsub("+", "\\texttt{+}", LABEL, fixed = T),
           # HEADER = if_else(HEADER %in% "Size of land holdings (ac)",
           #                  "\\specialcell{Size of land\\\\holdings (ac)}", HEADER),
           # HEADER = if_else(HEADER %in% "Size of forest holdings (ac)",
           #                  "\\specialcell{Size of forest\\\\holdings (ac)}", HEADER),
           HEADER = if_else(FOOTNOTE %in% "NOT_MUTUAL", paste0(HEADER, "$^b$"), HEADER),
           DESCRIPTION = gsub("<", "$<$", DESCRIPTION)) %>%
    arrange(SUBTABLE_NUMBER, ORDER)
  data.cont <- data[["cont"]] %>% filter(TABLE_NUMBER %in% tab.num)
  tab <- data.cat %>% select(TABLE) %>% distinct() %>% pull()
  sub <- data.cat %>% select(SUBTABLE_NUMBER) %>% distinct() %>% pull()
  h.sub <- data.cat %>% select(HEADER) %>% distinct() %>% pull()
  h <- data.cat %>% select(HEADER) %>% distinct() %>% pull()
  foot <- data.cat %>% select(FOOTNOTE) %>% distinct() %>% pull()

  geo.name <- ifelse(data.cat$GEO_ABB[1] %in% c("US", "WEST", "PACIFIC_COAST"),
                     paste0(data.cat$GEO_NAME[1], "$^{*}$"), data.cat$GEO_NAME[1])

  begin.tex <- c("\\pagebreak",
                 "\\begin{minipage}{6.5in}",
                 "\\raggedright",
                 if(!data.cat$DESCRIPTION[1] %in% "CONTINUED") {
                   c(paste0("\\hypertarget{", as.integer(data.cat$TABLE_NUMBER[1]), "}{\\hspace{20 mm}}\\\\"),
                     paste0("\\bookmark[page=\\thepage,level=0]{Table ", as.integer(data.cat$TABLE_NUMBER[1]), " -- ",
                          data.cat$TABLE_NAME[1], "}"))}
                 else "")

  caption <- paste0("{\\setlength\\textwidth{5in} \\noindent \\textbf{",
                    "Table ", gsub("_", "\\_", data.cat$GEO_ABB[1], fixed = T), "-",
                    as.integer(data.cat$TABLE_NUMBER[1])," (", year, "; ", stratum.abb, ", ", domain.abb, ").--",
                    if(!data.cat$DESCRIPTION[1] %in% "CONTINUED") {
                      paste0("Estimated area and estimated number of ",
                             tolower(stratum.name),  " (", tolower(domain.name), ") ",
                             "by \\emph{", data.cat$DESCRIPTION[1], "}, " , geo.name, ", ", year.range)}
                    else "continued",
                    "}}\\\\")

  body <- c("\\begin{center}",
            "\\begin{tabular}{p{1.5in} rr rr rr rr r}",
            "\\toprule",
            "\\multicolumn{1}{l}{\\multirow{2}{*}{\\begin{minipage}{1in} \\end{minipage} }} &\\multicolumn{4}{c}{\\textbf{Totals}} & \\multicolumn{4}{c}{\\textbf{Percentages}}\\\\",
            "\\cmidrule(r){2-5} \\cmidrule(l){6-9}",
            "& \\textbf{Acres} & \\textbf{SE$^a$} & \\textbf{\\specialcell{Owner-\\\\ships}} & \\textbf{SE$^a$} &",
            "\\textbf{Acres} & \\textbf{SE$^a$}& \\textbf{\\specialcell{Owner-\\\\ships}} & \\textbf{SE$^a$} & \\textbf{n}\\\\",
            "\\midrule",
            "& \\multicolumn{4}{c}{\\textit{- - - - - - - - thousands - - - - - - - }} & \\multicolumn{4}{c}{\\textit{- - - - - - - - percent - - - - - - - }}&\\\\",
            if(length(sub) == 1){
              c(paste0("\\raggedright ", h, " & & & & & & \\\\"),
                unlist(lapply(1:NROW(data.cat), nwos_table_row, data = data.cat)))}
            else{
              unlist(lapply(sub, function(j) {
                data <- data.cat %>% filter(SUBTABLE_NUMBER %in% j)
                c(paste0("\\raggedright ", h.sub[j], " & & & & & & \\\\"),
                  unlist(lapply(1:NROW(data), nwos_table_row, data = data)))}))},
            "\\bottomrule",
            "\\end{tabular}",
            "\\end{center}")

  footnotes <- c("\\vspace{-0.25in}",
                 "\\begin{center}",
                 "\\begin{minipage}[c]{6in}",
                 "{\\noindent \\raggedright \\hangindent=0.1in",
                 ifelse(data.cat$GEO_ABB[1] %in% c("US", "WEST", "PACIFIC_COAST"), "$^{*}$ Excluding Interior Alaska. \\\\", ""),
                 "$^a$ SE = standard error \\\\",
                 ifelse(foot == "NOT_MUTUAL", "$^b$ Categories are not mutually exclusive. \\\\", ""),
                 ifelse(tab == "SIZE",
                        paste0("The average (mean) forest holding size is ",
                               nwos_table_number(data.cont$MEAN, d = 1),
                               " acres per ownership (SE = ",
                               nwos_table_number(data.cont$MEAN_SE, d = 1, less.one = F),
                               "); median = ",
                               nwos_table_number(data.cont$MEDIAN),
                               " acres per ownership. \\\\"), ""),
                 ifelse(tab == "OWNTYPE_NUM",
                        paste0("The average (mean) number of owners is ",
                               nwos_table_number(data.cont$MEAN, d = 1),
                               " per ownership (SE = ",
                               nwos_table_number(data.cont$MEAN_SE, d = 1, less.one = F),
                               "); median = ",
                               nwos_table_number(data.cont$MEDIAN),
                               " owners per ownership. \\\\",
                               "The total number of owners is estimated to be ",
                               nwos_table_number(data.cont$OWNERS),
                               " (SE = ",
                               nwos_table_number(data.cont$OWNERS_SE),
                               "). \\\\"), ""),
                 ifelse(tab == "ACQ",
                        paste0("The average (mean) land tenure is ",
                               nwos_table_number(data.cont$MEAN, d = 1),
                               " years (SE = ",
                               nwos_table_number(data.cont$MEAN_SE, d = 1, less.one = F),
                               "); median = ",
                               nwos_table_number(data.cont$MEDIAN),
                               " years. \\\\"), ""),
                 ifelse(tab == "DEMO",
                        paste0("The average (mean) age is ",
                               nwos_table_number(data.cont$MEAN, d = 1),
                               " years (SE = ",
                               nwos_table_number(data.cont$MEAN_SE, d = 1, less.one = F),
                               "); median = ",
                               nwos_table_number(data.cont$MEDIAN),
                               " years. \\\\"), ""),
                 ifelse(tab == "INC_WOOD_CAT",
                        paste0("The average (mean) percentage income from forest land is ",
                               nwos_table_number(data.cont$MEAN, d = 1),
                               " (SE = ",
                               nwos_table_number(data.cont$MEAN_SE, d = 1, less.one = F),
                               "); median = ",
                               nwos_table_number(data.cont$MEDIAN, less.one = F),
                               " percent. \\\\"), ""),
                 "Note: Data may not add to totals due to rounding.",
                 "}",
                 "\\end{minipage}",
                 "\\end{center}")

  end.tex <- c("\\end{minipage}",
               "\\\\")

  c(begin.tex, caption, body, footnotes, end.tex)
}
