#' @name transf_impute_missing
#' @title Impute missing values
#' @author Nicolas Mangin
#' @description Impute numeric values with the mean and categorical values with the most frequent category.
#' @param x Numeric or character vector with missing values.
#' @return A vector of same class without missing values.
#' @export

transf_impute_missing <- function(x) {
  type <- class(x)
  if (type == "factor" | type == "character") {
    imputation <- names(which.max(table(x)))
  } else {
    imputation <- mean(x, na.rm = TRUE)
  }

  if (sum(is.na(x)) < length(x)) {
    x <- replace(x, is.na(x), imputation)
  }
  return(x)
}
