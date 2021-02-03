#' @name aggreg_cvar
#' @title Compute a coefficient of variation
#' @author Nicolas Mangin
#' @description Computes the coefficient of variation.
#' @param x numeric vector. Set of values for which the coefficient of variation should be computed.
#' @return A coefficient of variation for the numeric vector
#' @importFrom stats sd
#' @export

aggreg_cvar <- function(x) stats::sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
