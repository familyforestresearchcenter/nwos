#' nwos_table
#'
#' @param data
#' @details For area and cooperation rate tables see ...
#' nwos_table()
#' nwos_table_area()
#' nwos_table_coop()

nwos_table <- function(tab.num = 12.1, data = table.data,
                       stratum = stratum.name, domain = domain.name,
                       yr = year, yr.range = year.range) { 
  data <- data %>% filter(TABLE_NUMBER %in% tab.num,
                          !is.na(LABEL), !LABEL %in% "") %>%
    mutate(LABEL = gsub("+", "\\texttt{+}", LABEL, fixed = T),
           # HEADER = if_else(HEADER %in% "Size of land holdings (ac)", 
           #                  "\\specialcell{Size of land\\\\holdings (ac)}", HEADER),
           # HEADER = if_else(HEADER %in% "Size of forest holdings (ac)", 
           #                  "\\specialcell{Size of forest\\\\holdings (ac)}", HEADER),
           HEADER = if_else(FOOTNOTE %in% "NOT_MUTUAL", paste0(HEADER, "$^b$"), HEADER),
           DESCRIPTION = gsub("<", "$<$", DESCRIPTION)) %>%
    arrange(SUBTABLE_NUMBER, ORDER)
  sub <- data %>% select(SUBTABLE_NUMBER) %>% distinct() %>% pull()
  h.sub <- data %>% select(HEADER) %>% distinct() %>% pull()
  h <- data %>% select(HEADER) %>% distinct() %>% pull()
  foot <- data %>% select(FOOTNOTE) %>% distinct() %>% pull()
  dom <- "XX"
  if(domain %in% "1\\texttt{+} acres") dom <- "1\\texttt{+}"
  if(domain %in% "10\\texttt{+} acres") dom <- "10\\texttt{+}"
  if(domain %in% "100\\texttt{+} acres") dom <- "100\\texttt{+}"
  if(domain %in% "1,000\\texttt{+} acres") dom <- "1000\\texttt{+}"
  
  begin.tex <- c("\\pagebreak",
                 "\\begin{minipage}{6.5in}",
                 "\\raggedright",
                 if(!data$DESCRIPTION[1] %in% "CONTINUED") {
                   c(paste0("\\hypertarget{", as.integer(data$TABLE_NUMBER[1]), "}{\\hspace{20 mm}}\\\\"),
                     paste0("\\bookmark[page=\\thepage,level=0]{Table ", as.integer(data$TABLE_NUMBER[1]), " -- ",
                          data$TABLE_NAME[1], "}"))}
                 else "")
  
  caption <- paste0("{\\setlength\\textwidth{5in} \\noindent \\textbf{",
                    "Table ", data$GEO_ABB[1], "-", 
                    as.integer(data$TABLE_NUMBER[1])," (", yr, "; FFO ", dom, ")--",
                    if(!data$DESCRIPTION[1] %in% "CONTINUED") {
                      paste0("Estimated area and estimated number of ", 
                             tolower(stratum),  " (", tolower(domain), ") ",
                             "by ", data$DESCRIPTION[1], ", " , data$GEO_NAME[1], ", ", yr.range)}
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
                unlist(lapply(1:NROW(data), nwos_table_row, data = data)))}
            else{
              unlist(lapply(sub, function(j) {
                data <- data %>% filter(SUBTABLE_NUMBER %in% j)
                c(paste0("\\raggedright ", h.sub[j], " & & & & & & \\\\"),
                  unlist(lapply(1:NROW(data), nwos_table_row, data = data)))}))},
            "\\bottomrule",
            "\\end{tabular}",
            "\\end{center}")
  
  footnotes <- c("\\vspace{-0.25in}",
                 "\\begin{center}",
                 "\\begin{minipage}[c]{6in}",
                 "{\\noindent \\raggedright \\hangindent=0.1in",
                 "$^a$ SE = standard error \\\\",
                 ifelse(foot %in% "NOT_MUTUAL", "$^b$ Categories are not mutually exclusive \\\\", ""),
                 "Note: Data may not add to totals due to rounding",
                 "}",
                 "\\end{minipage}",
                 "\\end{center}")
  
  end.tex <- c("\\end{minipage}",
               "\\\\")
  
  c(begin.tex, caption, body, footnotes, end.tex)
}
