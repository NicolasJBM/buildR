#' Remove html tags, remove double spaces, convert to ASCII.
#' @param x      Tibble. One column "group", "period" and one column "value".
#' @param range  Integer. Number of years used to compute the slopes.
#' @param cutoff Numeric vector. Two values: minimum and maximum coefficient defining stability.
#' @param minsep Integer. Minimum number of periods of separation to justify a different phase.
#' @return A tibble with cycle phases identified for each group in each period.
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr lag
#' @importFrom dplyr summarise
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom tidyr unnest
#' @importFrom tidyr replace_na
#' @importFrom tidyr nest
#' @importFrom tibble tibble
#' @importFrom broom tidy
#' @importFrom purrr pmap
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom stats lm
#' @export


tble_get_cycle <- function(x,
                           range = 5,
                           cutoff = c(-0.5,0.5),
                           minsep = 3){
  
  group <- NULL
  period <- NULL
  value <- NULL
  term <- NULL
  start <- NULL
  end <- NULL
  lag1 <- NULL
  discontinuous <- NULL
  phase <- NULL
  estimate <- NULL
  trend <- NULL
  data <- NULL
  lagend <- NULL
  lagtrend <- NULL
  
  
  get_coeff <- function(gp, start, end, x, cutoff){
    y <- dplyr::filter(x, group == gp, period >= start, period <= end)
    broom::tidy(
      lm(
        value~period,
        data = dplyr::mutate(
          y,
          value = as.numeric(scale(value)),
          period = as.numeric(scale(period))
          )
        )
      ) %>%
      dplyr::filter(term == "period") %>%
      dplyr::mutate(group = gp, start = start, end = end) %>%
      dplyr::mutate(trend = dplyr::case_when(
        estimate <= cutoff[[1]] ~ -1,
        estimate > cutoff[[2]] ~ 1,
        TRUE ~ 0
      ))
  }
  
  time <- tibble::tibble(
    start = seq(from = min(x$period), to = max(x$period)-range+1, by = 1),
    end = seq(from = min(x$period)+range-1, to = max(x$period), by = 1)
  )
  
  base <- tibble(gp = unique(x$group), time = list(time)) %>%
    tidyr::unnest(time)
  
  get_phase <- function(x){
    y <- x %>%
      dplyr::mutate(lag1 = dplyr::lag(end, 1))
    
    y %>%
      tidyr::replace_na(list(lag1 = min(y$end)-1)) %>%
      dplyr::mutate(discontinuous = end - lag1 > minsep) %>%
      dplyr::mutate(phase = cumsum(discontinuous)) %>%
      dplyr::select(period = end, phase) %>%
      dplyr::mutate(phase = phase + 1) %>%
      dplyr::group_by(phase) %>%
      dplyr::summarise(start = min(period), end = max(period))
  }
  
  life_cycle <- purrr::pmap(base, get_coeff, x = x, cutoff = cutoff) %>%
    dplyr::bind_rows() %>%
    dplyr::select(group, start, end, estimate, trend) %>%
    dplyr::group_by(group, trend) %>%
    tidyr::nest() %>%
    dplyr::mutate(phase = purrr::map(data, get_phase)) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(phase) %>%
    dplyr::arrange(group, start, end) %>%
    dplyr::select(group, start, end, trend, phase) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(lagend = dplyr::lag(end), lagtrend = dplyr::lag(trend)) %>%
    dplyr::mutate(
      start = dplyr::case_when(!is.na(lagend) ~ lagend+1, TRUE ~ start),
      level = dplyr::case_when(
        trend == 0 & lagtrend == 1 ~ "High",
        trend == 0 & lagtrend == -1 ~ "Low",
        trend == 0 & lagtrend == NA ~ "Introduction",
        TRUE ~ ""
      )
    ) %>%
    dplyr::select(-lagend, -lagtrend) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(period = purrr::map2(start, end, function(x,y) c(x:y))) %>%
    dplyr::select(-start, -end) %>%
    tidyr::unnest(period)
  
  return(life_cycle)
}
