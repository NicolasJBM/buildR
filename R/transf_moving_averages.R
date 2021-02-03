#' @name transf_moving_averages
#' @title Compute moving average values
#' @author Nicolas Mangin
#' @description Compute moving averages or a chronological numeric vector
#' @param x     Numeric. Vector of chronological values to be transformed into moving averages.
#' @param range Integer. Number of periods before and after to average with the focal value.
#' @return A numeric vector of same size where values are smoothed.
#' @export

transf_moving_averages <- function(x, range) {
  
  stopifnot(
    length(x >= (2+range))
  )
  
  y <- x
  for (i in seq_len(length(y))) {
    if (i <= range) before <- range - i else before <- range
    if (i >= (length(y) - range)) after <- length(y) - i else after <- range
    xmin <- i - before
    xmax <- i + after
    y[i] <- mean(x[xmin:xmax], na.rm = TRUE)
  }
  return(y)
}
