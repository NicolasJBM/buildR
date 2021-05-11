#' @name transf_moving_aggregates
#' @title Compute moving aggregates
#' @author Nicolas Mangin
#' @description Compute moving averages or a chronological numeric vector
#' @param x     Numeric. Vector of chronological values to be transformed into moving averages.
#' @param range Integer. Number of periods before and after to average with the focal value.
#' @param fun   Function. Function to apply on the series (e.g. sum, mean, sd or any function taking a numeric vector as input and returns a numeric value)
#' @return A numeric vector of same size where values are smoothed.
#' @export

transf_moving_aggregates <- function(x, range, fun = NULL) {
  stopifnot(
    length(x) >= (2 + range),
    !is.null(fun)
  )
  y <- x
  for (i in seq_len(length(y))) {
    if (i <= range) before <- range - i else before <- range
    if (i >= (length(y) - range)) after <- length(y) - i else after <- range
    xmin <- max(0,i - before)
    xmax <- min(length(x), i + after)
    y[i] <- fun(x[xmin:xmax], na.rm = TRUE)
  }
  return(y)
}
