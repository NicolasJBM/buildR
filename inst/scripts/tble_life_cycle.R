library(dplyr)
library(ggplot2)

x1 <- tibble::tibble(period = seq(from = -10, to = 10, by = 0.1)) %>%
  dplyr::mutate(
    value = -period^2 + period + 200,
    random = (0.5-runif(nrow(.)))*50
  ) %>%
  mutate(total = value + random) %>%
  select(period, value = total) %>%
  mutate(period = 1:length(value), group = 1)

x2 <- tibble::tibble(period = seq(from = 0, to = 20, by = 0.1)) %>%
  dplyr::mutate(
    value = -period^2 + period + 150,
    random = (0.5-runif(nrow(.)))*50
  ) %>%
  mutate(total = value + random) %>%
  select(period, value = total) %>%
  mutate(period = 1:length(value), group = 2)

x <- bind_rows(x1, x2)

rm(x1,x2)


span <- 25
cutoff = c(-0.5,0.5)


get_coeff <- function(gp, start, end, x, cutoff){
  y <- dplyr::filter(x, group == gp, period >= start, period <= end)
  broom::tidy(lm(value~period, data = y)) %>%
    filter(term == "period") %>%
    mutate(group = gp, start = start, end = end) %>%
    mutate(trend = case_when(
      estimate <= cutoff[[1]] ~ "Decline",
      estimate > cutoff[[2]] ~ "Growth",
      TRUE ~ "Stability"
    ))
}


time <- tibble(
  start = seq(from = min(x$period), to = max(x$period)-span+1, by = 1),
  end = seq(from = min(x$period)+span-1, to = max(x$period), by = 1)
)

base <- tibble(gp = unique(x$group), time = list(time)) %>%
  tidyr::unnest(time)

result <- purrr::pmap(base, get_coeff, x = x, cutoff = cutoff) %>%
  bind_rows() %>%
  select(group, start, end, estimate, trend)





View(tidyr::spread(select(result, group, end, trend), group, trend))
ggplot(x, aes(x = period, y = value, group = group, color = group)) + geom_smooth(method = "loess")
