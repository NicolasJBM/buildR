#' Impute numeric values with the mean and categorical values with the most frequent category.
#' @param x dataframe with missing values.
#' @return A dataframe without missing values.
#' @export

stat_fill_na <- function(x){
  
  for (i in 1:length(x)){
    type <- class(x[[i]])
    if (type == "factor") value <- names(which.max(table(x[[i]]))) else value <- mean(x[[i]], na.rm = T)
    if (sum(is.na(x[[i]])) < length(x[[i]])) x[i] <- replace(x[i], is.na(x[i]), value)
  }
  
  x
}