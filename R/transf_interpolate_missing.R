#' @name transf_interpolate_missing
#' @title Interpolate or extrapolate missing values
#' @author Nicolas Mangin
#' @description Replace missing values with interpolation (or extrapolation) over five years.
#' @param x     Numeric. Vector of chronological values with missing data.
#' @param range Integer. Number of periods before and after used to interpolate with the focal value.
#' @return A tibble with year and actual or interpolated values
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr lag
#' @export


transf_interpolate_missing <- function(x, range){
  
  stopifnot(
    length(x >= (2+range))
  )
  
  y <- x
  for (i in seq_len(length(y))) {
    if (is.na(y[i])){
      if (i <= range) before <- range - i else before <- range
      if (i >= (length(y) - range)) after <- length(y) - i else after <- range
      xmin <- i - before
      xmax <- i + after
      y[i] <- mean(x[xmin:xmax], na.rm = TRUE)
    }
  }
  return(y)
}
