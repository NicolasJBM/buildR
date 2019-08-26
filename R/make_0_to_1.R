#' Transform all the numeric variables in a dataframe so that their values are between 0 and 1 (impute with 0 or 1 if there is no variation).
#' @param x dataframe. Database with numeric variables to scale between 0 and 1.
#' @return A dataframe where all numeric variables are between 0 and 1.
#' @importFrom dplyr mutate_if
#' @export

make_0_to_1 <- function(x) {
  x %>% mutate_if(is.numeric, scale01)
}

scale01 <- function(x) {
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    x <- min(1, max(x))
  } else {
    x <- (x-min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
  }
  x
}