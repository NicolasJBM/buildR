#' @name transf_discretize
#' @title Transform a continuous variable into a discrete one
#' @author Nicolas Mangin
#' @description  Transform a continuous variable into a discrete one.
#' @param x      Numeric. Vector of values to be transformed into a discrete ordinal variable.
#' @param method character. Specify whether the variables should be discretized using quantiles ("quantiles") or kmeans ("kmeans")
#' @param nval   Integer. Number of discrete values desired for each variable (groups = 2 makes binary variables).
#' @return A dataframe where all variables are discrete.
#' @importFrom dplyr mutate_if
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr right_join
#' @importFrom dplyr ntile
#' @importFrom stats kmeans
#' @importFrom stats quantile
#' @export


transf_discretize <- function(x, method = "quantiles", nval = 2) {
  stopifnot(
    is.numeric(x),
    is.integer(nval),
    method %in% c("quantiles", "kmeans")
  )

  if (method == "kmeans") {
    nval <- min((length(unique(x)) - 1), nval)
    if (nval > 0) {
      y <- as.numeric(stats::kmeans(x, centers = nval)$cluster) - 1
    } else {
      y <- rep(0, length(x))
    }
  } else {
    breaks <- c()
    for (i in 1:(nval - 1)) breaks[i] <- stats::quantile(x, i / nval)
    breaks <- c(min(x) - 1, breaks, max(x) + 1)
    y <- cut(x, breaks = breaks) %>%
      as.numeric()
    y <- y - 1
  }
  return(y)
}
