#' tjur
#'
#' Calculate Tjur's Statistic
#'
#' @details
#' Calculates Tjur's statistic for predictions from a binary model
#'
#' @param data is a data frame
#' @param variable is the name of the binary response variable
#' @param pred.prob is the predicted probability
#'
#' @examples
#' tjur(data = model.data, variable = "RESPONSE_CD", pred.prob = "PRED")
#'
#' @export

tjur <- function(data, variable = "RESPONSE_CD", pred.prob = "PRED") {
  abs(mean(data %>%
             filter((!!as.symbol(variable)) == "1") %>%
             pull((!!as.symbol(pred.prob)))) -
        mean(data %>%
               filter((!!as.symbol(variable)) == "0") %>%
               pull((!!as.symbol(pred.prob)))))}
