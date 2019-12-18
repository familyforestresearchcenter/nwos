#' get_nwos_db_catalog
#'
#' Generates NWOS database catalog
#'
#' @details
#' This function generates an up-to-date HTML version of the NWOS database catalog.
#'
#' @param dir is a string vector containing the path where the catalog will be generated.
#' @param file is a string vector containing the file name (without file extension) that will be generated.
#'
#' @examples
#' get_nwos_db_catalog(dir='C:/foo',file='NWOS_DB_catalog')
#'
#' @export

get_nwos_db_catalog <- function(dir='.',file='NWOS_DB_catalog'){

  #packages
  library(rmarkdown)

  #find template
  t <- system.file("NWOS_DB_catalog.Rmd", package="nwos")

  new <- paste(dir,'/',file,'.Rmd',sep='') #path to new location
  file.copy(t,new) #copy to new location

  render(new) #render Rmd file to html

  unlink(new) #delete Rmd file

}
