#' Round all numeric entries in a data frame
#'
#' @param df A data.frame
#' @param digits Number of digits to round numerical entries to.
#'
#' @export
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}

#' Take two numerical vectors and output a dataframe containing the OLS slope and intercept
#'
#' @param x Independent variable.
#' @param y Dependent variable.
#'
#' @export
get_lm <- function(x,y) {
  data <- tibble(x,y)
  names(data) <- c('x','y')
  data_out <- data %>%
    summarize(slope = (t(x-mean(x))%*%(y-mean(y))/(length(y)-1))/(sum((x - mean(x))^2)/(length(x)-1)),
              intercept = mean(y)-slope*mean(x))
  return(data_out)
}

#' Equivalent of the "left" function in Excel
#' 
#' @param x A text string.
#' @param n Number of characters to extract starting from the beginning of the string.
#' 
#' @export
left <- function(x, n) {
  substr(x, 1, n)
}

#' Equivalent of the "right" function in Excel
#' 
#' @param x A text string.
#' @param n Number of characters to extract starting from the end of the string counting backwards.
#' 
#' @export
right <- function(x, n) {
  substr(x, nchar(x) - (n-1), nchar(x))
}
