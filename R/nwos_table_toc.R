#' nwos_table_toc
#' @export

nwos_table_toc <- function(table.number, toc.data, domain, domain.abb) {
  toc.data <- toc.data %>% filter(TABLE_NUMBER %in% table.number)

  if(table.number %in% "1") { # Area
    tex <- c("\\bigskip",
             paste0("\\hyperlink{", toc.data$TABLE_NUMBER, "}{",
                    "Table ", gsub("_", "\\_", toc.data$GEO_ABB, fixed = T), "-",
                    toc.data$TABLE_NUMBER," (", toc.data$YEAR,")--",
                    "Estimated area of forest land by ownership category, ",
                    toc.data$GEO_NAME, ", ", toc.data$YEAR_RANGE,
                    "}\\\\"))
  }
  if(table.number %in% "2"){ # Sample size
    tex <- c("\\bigskip",
             paste0("\\hyperlink{", toc.data$TABLE_NUMBER, "}{",
                    "Table ", gsub("_", "\\_", toc.data$GEO_ABB, fixed = T), "-",
                    toc.data$TABLE_NUMBER," (", toc.data$YEAR,"; FFO 1\\texttt{+})--",
                    "Sample size and cooperation rate for family forest ownerships ",
                    "for the USDA Forest Service, National Woodland Owner Survey, ",
                    toc.data$GEO_NAME, ", ", toc.data$YEAR_RANGE,
                    "}\\\\"))
  }
  if(table.number %in% "3"){ # Total
    tex <- c("\\bigskip",
             paste0("\\hyperlink{", toc.data$TABLE_NUMBER, "}{",
                    "Table ", gsub("_", "\\_", toc.data$GEO_ABB, fixed = T), "-",
                    toc.data$TABLE_NUMBER," (", toc.data$YEAR, "; FFO ", domain.abb, ")--",
                    "Total estimated area and estimated number of ",
                    tolower(toc.data$STRATUM_NAME),  " (", tolower(toc.data$DOMAIN_NAME), ") ",
                    ", " , toc.data$GEO_NAME, ", ", toc.data$YEAR_RANGE,
                    "}\\\\"))
  }
  if(!table.number %in% 1:3) {
    tex <- c("\\bigskip",
             paste0("\\hyperlink{", toc.data$TABLE_NUMBER, "}{",
                    "Table ", gsub("_", "\\_", toc.data$GEO_ABB, fixed = T), "-",
                    toc.data$TABLE_NUMBER," (", toc.data$YEAR, "; FFO ", domain.abb, ")--",
                    "Estimated area and estimated number of ",
                    tolower(toc.data$STRATUM_NAME),  " (", tolower(toc.data$DOMAIN_NAME), ") ",
                    "by \\emph{", toc.data$DESCRIPTION, "}, " , toc.data$GEO_NAME, ", ", toc.data$YEAR_RANGE,
                    "}\\\\"))
  }
  return(tex)
}
