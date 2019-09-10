#' Computes the coefficient of variation.
#' @param x numeric vector. Set of values for which the coefficient of variation should be computed.
#' @return A coefficient of variation for the numeric vector
#' @importFrom stats sd
#' @export

stat_cvar <- function(x) sd(x, na.rm = T) / mean(x, na.rm = T)