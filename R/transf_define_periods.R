#' @name transf_define_periods
#' @title Define periods
#' @author Nicolas Mangin
#' @description Find period characteristics: phase, level, evolution.
#' @param x      Numeric. Vector of chronological values to be transformed into periods
#' @param range  Integer. Number of years after the focal value used to compute slopes.
#' @param cutoff Double. Absolute slope below which the period is considered stable.
#' @return A tibble with phases identified: phase, level, evolution.
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom tibble tibble
#' @importFrom stats quantile
#' @importFrom stats lm
#' @export


transf_define_periods <- function(x,
                                  range = 2,
                                  cutoff = c(-0.5, 0.5)) {
  stopifnot(
    length(x >= (2 * range))
  )

  y <- x

  for (i in seq_len(length(y))) {
    if (i >= (length(y) - range)) {
      after <- length(y) - i
      before <- i + range - length(y)
    } else {
      after <- range
      before <- 0
    }
    start <- i - before
    end <- i + after
    values <- x[start:end]
    periods <- seq_len(length(values))
    regr <- stats::lm(values ~ periods)
    y[i] <- regr$coefficients[2]
  }

  quantiles_periods <- stats::quantile(seq_len(length(x)), c(0.333, 0.6666, 1))
  quantiles_values <- stats::quantile(x, c(0.333, 0.6666, 1))

  phases <- tibble::tibble(
    period = seq_len(length(x)),
    value = x,
    slope = y
  ) %>%
    dplyr::mutate(
      phase = dplyr::case_when(
        period <= quantiles_periods[1] ~ "start",
        period >= quantiles_periods[3] ~ "middle",
        TRUE ~ "end"
      ),
      level = dplyr::case_when(
        value <= quantiles_values[1] ~ "low",
        value >= quantiles_values[3] ~ "high",
        TRUE ~ "medium"
      ),
      evolution = dplyr::case_when(
        slope <= -cutoff ~ "decline",
        slope >= cutoff ~ "growth",
        TRUE ~ "stability"
      )
    )

  return(phases)
}
