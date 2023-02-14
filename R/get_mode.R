#' get_mode
#'
#' Find the modal value for a vector
#'
#' @details
#' Finds the most common value. If there is a tie, the value that
#' is first alphabetically is taken.
#'
#' @param x is a vector
#'
#' @examples
#' get_mode(c("a", "a", "b"))
#' get_mode(c("a", "b", "b"))
#' get_mode(c("a", "a", "b", "b"))
#' get_mode(c("b", "b", "a", "a"))
#'
#' @export

get_mode <- function(x) {
  unique.x <- sort(unique(x))
  unique.x[which.max(tabulate(match(x, unique.x)))]
  }
