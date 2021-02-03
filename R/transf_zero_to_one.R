#' @name transf_zero_to_one
#' @title Tranform from 0 to 1
#' @author Nicolas Mangin
#' @description Transform a numeric variable so that its values are between 0 and 1 (impute with 1 if no variation)
#' @param x Numeric. Values to be transformed into 0 to 1 values
#' @return A numeric variables going from 0 and 1.
#' @export

transf_zero_to_one <- function(x) {
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    x <- 1
  } else {
    x <- (x - min(x, na.rm = TRUE)) /
      (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  x
}
