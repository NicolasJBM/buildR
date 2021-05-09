#' @name aggreg_blau
#' @title Compute a Blau index
#' @author Nicolas Mangin
#' @description Computes the Blau index.
#' @param x numeric vector. Set of values (0 or 1) for which the Blau index should be computed.
#' @param na.rm Logical. Should NAs be omitted.
#' @return A Blau index for the numeric vector
#' @export

aggreg_blau <- function(x, na.rm = TRUE) {
  p <- mean(x, na.rm = na.rm)
  q <- 1-p
  1-p^2-q^2
}
