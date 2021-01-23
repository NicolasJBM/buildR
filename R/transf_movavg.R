#' Computes the coefficient of variation.
#' @param x     numeric vector. Set of values to be transformed into moving averages.
#' @param range Integer. Number of (prior) periods to aggregate.
#' @return A numeric vector of same size where values are smoothed.
#' @export


stat_movavg <- function(x, range){
  y <- x
  for (i in 1:length(y)){
    gap <- i-range
    xmin <- max(1, i-range+1)
    xmax <- max(i, i-gap)
    y[i] <- mean(x[xmin:xmax], na.rm = TRUE)
  }
  return(y)
}
