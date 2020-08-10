#' nwos_table
#'
#' Create the body of an NWOS core, area, or cooperataion rate table
#' @export
#' @details For area and cooperation rate tables see ...
#' geo.abb = "AL"
#' data = AREA
#' yr = year
#' yr.range = year.range
#' ref.tab = REF_TABLE
#' ref.geo = REF_GEO
#' nwos_table()
#' nwos_table_area(geo.abb = "US")
#' nwos_table_area("TX")

nwos_table_area <- function(geo.abb, data = AREA,
                            year = YEAR, year.range = YEAR_RANGE,
                            ref.tab = REF_TABLE, ref.geo = REF_GEO) {
  ref.geo <- ref.geo %>% filter(GEO_ABB %in% geo.abb)
  ref.tab <- ref.tab %>% filter(TABLE %in% "FOREST_AREA")

  geo.name <- ifelse(ref.geo$GEO_ABB %in% c("US", "WEST", "PACIFIC_COAST"),
                     paste0(ref.geo$GEO_NAME, "$^{*}$"), ref.geo$GEO_NAME)

  data <- data %>% filter(GEO_ABB %in% geo.abb) %>%
    select(OWNGRP, ACRES, ACRES_VARIANCE)
  data <- bind_rows(data,
                    tibble(OWNGRP = "Private",
                           data %>% filter(OWNGRP %in% c("Family", "Corporate", "Other private")) %>%
                             summarize_at(vars(ACRES, ACRES_VARIANCE), sum)),
                    tibble(OWNGRP = "Public",
                           data %>% filter(OWNGRP %in% c("Federal", "State", "Local")) %>%
                             summarize_at(vars(ACRES, ACRES_VARIANCE), sum)),
                    tibble(OWNGRP = "Total",
                           data %>% summarize_at(vars(ACRES, ACRES_VARIANCE), sum)))

  begin.tex <- c("\\pagebreak",
                 "\\begin{minipage}{6.5in}",
                 "\\raggedright",
                 paste0("\\hypertarget{", ref.tab$TABLE_NUMBER, "}{\\hspace{20 mm}}\\\\"),
                 paste0("\\bookmark[page=\\thepage,level=0]{Table ", ref.tab$TABLE_NUMBER, " -- ",
                        ref.tab$TABLE_NAME, "}"))

  caption <- paste0("{\\setlength\\textwidth{5in} \\noindent \\textbf{",
                    "Table ", gsub("_", "\\_", ref.geo$GEO_ABB, fixed = T), "-",
                    ref.tab$TABLE_NUMBER," (", year,").--",
                    "Estimated area of forest land by ownership category, ",
                    geo.name, ", ", year.range,
                    "}}\\\\")

  body <- c("\\begin{center}",
            "\\begin{tabular}{l rr}",
            "\\toprule",
            "\\textbf{\\specialcell{Ownership\\\\category}} & \\textbf{Acres} & \\textbf{SE$^a$} \\\\",
            '\\midrule',
            "& \\multicolumn{2}{c}{\\textit{- - thousands - -}} \\\\",
            "Private \\\\",
            "\\rowcolor{gray!25}",
            paste0("\\hspace{0.5em} Family  & ",
                   nwos_table_number(data %>% filter(OWNGRP %in% "Family") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNGRP %in% "Family") %>% pull(ACRES_VARIANCE)) / 1e3),
                   " \\\\"),
            paste0("\\hspace{0.5em} Corporate  & ",
                   nwos_table_number(data %>% filter(OWNGRP %in% "Corporate") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNGRP %in% "Corporate") %>% pull(ACRES_VARIANCE)) / 1e3),
                   " \\\\"),
            "\\rowcolor{gray!25}",
            paste0("\\hspace{0.5em} Other private  & ",
                   nwos_table_number(data %>% filter(OWNGRP %in% "Other private") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNGRP %in% "Other private") %>% pull(ACRES_VARIANCE)) / 1e3),
                   " \\\\"),
            "\\cmidrule{2-3}",
            paste0("\\hspace{0.5em} Total private  & ",
                   nwos_table_number(data %>% filter(OWNGRP %in% "Private") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNGRP %in% "Private") %>% pull(ACRES_VARIANCE)) / 1e3),
                   " \\\\"),
            "Tribal \\\\",
            "\\rowcolor{gray!25}",
            paste0("\\hspace{0.5em} Tribal  & ",
                   nwos_table_number(data %>% filter(OWNGRP %in% "Tribal") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNGRP %in% "Tribal") %>% pull(ACRES_VARIANCE)) / 1e3),
                   " \\\\"),
            "Public \\\\",
            "\\rowcolor{gray!25}",
            paste0("\\hspace{0.5em} Federal  & ",
                   nwos_table_number(data %>% filter(OWNGRP %in% "Federal") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNGRP %in% "Federal") %>% pull(ACRES_VARIANCE)) / 1e3),
                   " \\\\"),
            paste0("\\hspace{0.5em} State  & ",
                   nwos_table_number(data %>% filter(OWNGRP %in% "State") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNGRP %in% "State") %>% pull(ACRES_VARIANCE)) / 1e3),
                   " \\\\"),
            '\\rowcolor{gray!25}',
            paste0("\\hspace{0.5em} Local  & ",
                   nwos_table_number(data %>% filter(OWNGRP %in% "Local") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNGRP %in% "Local") %>% pull(ACRES_VARIANCE)) / 1e3),
                   " \\\\"),
            "\\cmidrule{2-3}",
            paste0("\\hspace{0.5em} Total public  & ",
                   nwos_table_number(data %>% filter(OWNGRP %in% "Public") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNGRP %in% "Public") %>% pull(ACRES_VARIANCE)) / 1e3),
                   " \\\\"),
            "\\cmidrule{2-3}",
            "\\rowcolor{gray!25}",
            paste0("\\hspace{0.5em} Total  & ",
                   nwos_table_number(data %>% filter(OWNGRP %in% "Total") %>% pull(ACRES) / 1e3), "  & ",
                   nwos_table_number(sqrt(data %>% filter(OWNGRP %in% "Total") %>% pull(ACRES_VARIANCE)) / 1e3),
                   " \\\\"),
            "\\bottomrule \\\\",
            "\\end{tabular}",
            "\\end{center}")

  footnotes <- c("\\vspace{-0.25in}",
                 "\\begin{center}",
                 "\\begin{minipage}[c]{3.25in}",
                 "{\\noindent \\raggedright \\hangindent=0.1in",
                 ifelse(ref.geo$GEO_ABB %in% c("US", "WEST", "PACIFIC_COAST"), "$^{*}$ Excluding Interior Alaska. \\\\", ""),
                 "$^a$ SE = standard error\\\\",
                 "Note: Data may not add to totals due to rounding.",
                 "}",
                 "\\end{minipage}",
                 "\\end{center}")

  end.tex <- c("\\end{minipage}",
               "\\\\")

  c(begin.tex, caption, body, footnotes, end.tex)
}
