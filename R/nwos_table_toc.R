#' nwos_table_toc
#' @export

nwos_table_toc <- function(table.number, toc.data, domain) {
  toc.data <- toc.data %>% filter(TABLE_NUMBER %in% table.number)
  dom <- "XX"
  if(domain %in% "1\\texttt{+} acres") dom <- "1\\texttt{+}"
  if(domain %in% "10\\texttt{+} acres") dom <- "10\\texttt{+}"
  if(domain %in% "100\\texttt{+} acres") dom <- "100\\texttt{+}"
  if(domain %in% "1,000\\texttt{+} acres") dom <- "1000\\texttt{+}"

  if(table.number %in% "1") { # Area
    tex <- c("\\bigskip",
             paste0("\\hyperlink{", toc.data$TABLE_NUMBER, "}{",
                    "Table ", toc.data$GEO_ABB, "-",
                    toc.data$TABLE_NUMBER," (", toc.data$YEAR,")--",
                    "Estimated area of forest land by ownership category, ",
                    toc.data$GEO_NAME, ", ", toc.data$YEAR_RANGE,
                    "}\\\\"))
  }
  if(table.number %in% "2"){ # Sample size
    tex <- c("\\bigskip",
             paste0("\\hyperlink{", toc.data$TABLE_NUMBER, "}{",
                    "Table ", toc.data$GEO_ABB, "-",
                    toc.data$TABLE_NUMBER," (", toc.data$YEAR,"; FFO 1\\texttt{+})--",
                    "Sample size and cooperation rate for family forest ownerships ",
                    "for the USDA Forest Service, National Woodland Owner Survey, ",
                    toc.data$GEO_NAME, ", ", toc.data$YEAR_RANGE,
                    "}\\\\"))
  }
  if(table.number %in% "3"){ # Total
    tex <- c("\\bigskip",
             paste0("\\hyperlink{", toc.data$TABLE_NUMBER, "}{",
                    "Table ", toc.data$GEO_ABB, "-",
                    toc.data$TABLE_NUMBER," (", toc.data$YEAR, "; FFO ", dom, ")--",
                    "Total estimated area and estimated number of ",
                    tolower(toc.data$STRATUM_NAME),  " (", tolower(toc.data$DOMAIN_NAME), ") ",
                    ", " , toc.data$GEO_NAME, ", ", toc.data$YEAR_RANGE,
                    "}\\\\"))
  }
  if(!table.number %in% 1:3) {
    tex <- c("\\bigskip",
             paste0("\\hyperlink{", toc.data$TABLE_NUMBER, "}{",
                    "Table ", toc.data$GEO_ABB, "-",
                    toc.data$TABLE_NUMBER," (", toc.data$YEAR, "; FFO ", dom, ")--",
                    "Estimated area and estimated number of ",
                    tolower(toc.data$STRATUM_NAME),  " (", tolower(toc.data$DOMAIN_NAME), ") ",
                    "by ", toc.data$DESCRIPTION, ", " , toc.data$GEO_NAME, ", ", toc.data$YEAR_RANGE,
                    "}\\\\"))
  }
  return(tex)
}
