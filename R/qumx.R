#' qumx
#'
#' Generates the appropriate SQL query for selecting the most recent record in a table (the record with the numerically highest CN number) in the NWOS DB
#'
#' @param x is a character string containing the name of an NWOS table (without schema named appended)
#'
#' @return a character string
#'
#' @examples
#' qumx('SAMPLE')
#'
#' @export

qumx <- function(x){
	query <- "SELECT * FROM FS_NWOS.<TABTAG> WHERE CAST(SUBSTR(CN,4) AS NUMBER) = (SELECT MAX(CAST(SUBSTR(CN,4) AS NUMBER)) FROM FS_NWOS.<TABTAG>)"
	gsub("<TABTAG>",x,query)
	}
